const std = @import("std");

const AnyReader = std.io.AnyReader;
const Allocator = std.mem.Allocator; 
const ArenaAllocator = std.heap.ArenaAllocator;


const Self = @This();

pub const ObjLoadErrors = error{

};


const DirectiveType = enum(u8) {
    Vertex,
    TexCoord,
    Normal,
    Face,
    Name,
    UseMat,
    MatLib,
};

pub const TokenType = enum(u8) {
    Directive,
    String,
    FloatingPoint,
    FaceDef,
    EOF,

};


const Token = union(TokenType) {
    const dir_map = std.StaticStringMap(DirectiveType).initComptime(.{
        .{"v", .Vertex},
        .{"vt", .TexCoord},
        .{"vn", .Normal},
        .{"f", .Face},
        .{"o", .Name},
        .{"usemtl", .UseMat},
        .{"mtllib", .MatLib},
    });

    Directive: DirectiveType,
    String: []const u8,
    FloatingPoint: f32,
    FaceDef: [3]u32,
    EOF,

    pub fn eql(self: Token, other: Token) bool {
        const t1 = std.meta.activeTag(self);
        const t2 = std.meta.activeTag(other);

        if (t1 != t2) return false;

        return switch (self) {
            .Directive => other.Directive == self.Directive,
            .String => std.mem.eql(self.String, other.String),
            .FloatingPoint => self.FloatingPoint == other.FloatingPoint,
            .EOF => return t2 == .EOF,

            .FaceDef => |faces| {
                for (0..faces.len) |i| {
                    if (faces[i] != other.FaceDef[i]) {
                        return false;
                    }
                }

                return  true;
            },
        };
    }

    pub fn getType(self: Token) TokenType {
        return switch (self) {
            .Directive => .Directive,
            .String => .String,
            .FloatingPoint => .FloatingPoint,
            .FaceDef => .FaceDef,
        };
    }

    fn parseFaceDef(raw: []const u8) ?[3]u32 {
        var split = std.mem.splitScalar(u8, raw, '/'); 
        
        var faces = [_]u32{0} ** 3;
        var ind: usize = 0;
        while (split.next()) |part| : (ind += 1) {
            // 0 is a valid "null" value in this case since .obj face defs are
            // 1 indexed because of course they are
            faces[ind] = std.fmt.parseInt(u32, part, 0) catch 0; 
        }

        return faces;

    }

    pub fn fromRaw(raw: []const u8, allocator: Allocator) !Token {
        if (dir_map.get(raw)) |v| {
            return Token{.Directive = v};
        }

        const fp: ?f32 = std.fmt.parseFloat(f32, raw) catch null;
        if (fp) |v| {
            return Token{.FloatingPoint = v};
        }

        const face: ?[3]u32 = parseFaceDef(raw);
        if (face) |f| {
            return Token{.FaceDef = f};
        }

        const str = try allocator.alloc(u8, raw.len);
        @memcpy(str, raw);

        return Token{.String = str};
    }
};

const TokenList = std.ArrayListUnmanaged(Token);

const buf_size: usize = 1024;


allocator: Allocator,

//TODO: Also provide some map and collect functions to remap the actual outputted attributes

// lazily evaluated parsed data

// The parser always maintains ownership of tokens once they are generated
//
tokens: TokenList,

// temporary allocator for parsed symbols
// -- destroyed when loaded data ownership is transferred or on deinit
str_allocator: ArenaAllocator,


pub fn fromFilePath(path: []const u8, allocator: Allocator) ObjLoadErrors!Self {
    const file = try std.fs.cwd().openFile(path, .{.mode = .read_only});
    defer file.close();

    const reader = file.reader().any();

    return try fromReader(reader, allocator);
}

pub fn fromReader(reader: AnyReader, allocator: Allocator) ObjLoadErrors!Self {

    var loader = Self{
        .allocator = allocator,
        .str_allocator = ArenaAllocator.init(allocator),
        .tokens = try TokenList.initCapacity(allocator, 64),
    };

    try loader.tokenize(reader);

    return loader;
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.buffer);
    self.str_allocator.deinit();
    self.tokens.deinit(self.allocator);
}

const TokenizerState = struct {
    buf: []u8,
    buf_size: usize = 0,
    pos: usize = 0,
    file: std.fs.File,

    /// ## Notes:
    /// - A return value of null indicates the EOF token...
    pub fn nextTokenRaw(self: *TokenizerState) ?[]u8 {
        // Stop conditions: 
        //  - Whitespace character encountered
        //  - End of File encountered
        //      - reach buf size position, re read, then buf size = 0
        //
        //  Special Case: 
        //  - End of buffer in the middle of token
        //      - Handle this by refilling the buffer at an offset and resetting the token indices
        //  - Returned token slice is valid until the next token is retrieved, since the buffer
        //    could refill then

        // eat preceding whitespace
        while (std.ascii.isWhitespace(self.buf[self.pos])) : (self.pos += 1) {
            if (self.pos == self.buf_size) {
                self.pos = 0;

                self.buf_size = try self.file.read(self.buf);

                if (self.buf_size == 0) {
                    return null;
                }
            }
        }

        var sym_begin = self.pos;
        var sym_end = self.pos;

        while (!std.ascii.isWhitespace(self.buf[self.pos]) and self.buf_size != 0) : (sym_end += 1) {
            if (self.pos == self.buf_size) {
                const sym_len: u64 = @intCast(sym_end - sym_begin);
                const fpos = try self.file.getPos();

                try self.file.seekTo(fpos - sym_len);
                self.buf_size = try self.file.read(self.buf);

                sym_begin = 0;
                sym_end = sym_len;

                self.pos = 0;
            }

            self.pos += 1;
        }

        if (self.buf_size == 0) {
            return null;
        }


    }
};


fn tokenize(self: *Self, reader: AnyReader) ObjLoadErrors!void {

    // Do tokenization stuff
    const allocator = self.str_allocator.allocator();
    const buf = try allocator.alloc(u8, buf_size);
    defer self.allocator.free(buf);

    var tokenizer = TokenizerState{
        .buf = buf,
        .reader = reader,
    };

    while (tokenizer.nextTokenRaw()) |raw| {
        const tok = try Token.fromRaw(raw, allocator);
        try self.tokens.append(allocator, tok);
    }

    try self.tokens.append(allocator, .{.EOF});
}


const log = std.log.scoped(.obj_loader);

fn validateAgainstTokens(self: *const Self, tokens: []const Token) !void {
    for (self.tokens.items, 0..) |tok, index| {
        if (!tok.eql(tokens[index])) return error.MismatchedToken;
    }
}

fn expectEqual(tok_str: []const u8, b: Token, allocator: Allocator) !void {
    const tok = try Token.fromRaw(tok_str, allocator);
    if (!tok.eql(b)) return error.MismatchedToken;
}


fn makeTokens(comptime values: anytype) []const Token {
    comptime var tokens: []const Token = &.{};

    inline for (values) |val| {
        tokens = tokens ++ [_]Token{
            switch(@TypeOf(val)) {
                DirectiveType => Token{.Directive = val},

                f32 => Token{.FloatingPoint = val},
                
                struct {u32, u32, u32} => Token{.FaceDef = [3]u32{val[0], val[1], val[2]}},

                []const u8 => Token{.String = val},
            }
        };
    }

    tokens = tokens ++ [_]Token{.{.EOF}};

    return tokens;
}

test "individual tokenization" {
    var test_arena = ArenaAllocator.init(std.heap.page_allocator);
    defer test_arena.deinit();

    const allocator = test_arena.allocator();

    // validate stuff
    try expectEqual("v", .{.Directive = .Vertex}, allocator);
    try expectEqual("vt", .{.Directive = .TexCoord}, allocator);
    try expectEqual("vn", .{.Directive = .Normal}, allocator);
    try expectEqual("1/2/3", .{.FaceDef = [3]u32{1, 2, 3}}, allocator);
    try expectEqual("1//2", .{.FaceDef = [3]u32{1, 0, 2}}, allocator);

    //NOTE: Likely issue with the exisitng parser scheme
    // (Ambiguity between single face def and floating point number)
    try expectEqual("1", .{.FaceDef = [_]u32{1, 0, 0}}, allocator);
    try expectEqual("1/2", .{.FaceDef = [_]u32{1, 2, 0}}, allocator);
    try expectEqual("Cube", .{.String = "Cube"}, allocator);
    try expectEqual("-1.02", .{.FloatingPoint = -1.02}, allocator);
}

test "valid file" {
    var test_arena = ArenaAllocator.init(std.heap.page_allocator);
    defer test_arena.deinit();

    const allocator = test_arena.allocator();

    // Validate the raw data is read in the correct order
    var data: Self = try Self.fromFilePath("test/cube.obj", allocator, .Lazy);
    try data.validateAgainstTokens(makeTokens(.{
        .MatLib, "cube.mtl",
        .Name, "Cube",
        .Vertex, 1.000000, 1.000000, -1.000000,
        .Vertex, 1.000000, -1.000000, -1.000000,
        .Vertex, 1.000000, 1.000000, 1.000000,
        .Vertex, 1.000000, -1.000000, 1.000000,
        .Vertex, -1.000000, 1.000000, -1.000000,
        .Vertex, -1.000000, -1.000000, -1.000000,
        .Vertex, -1.000000, 1.000000, 1.000000,
        .Vertex, -1.000000, -1.000000,1.000000,

        .TexCoord, 0.625000, 0.500000,
        .TexCoord, 0.875000, 0.500000,
        .TexCoord, 0.875000, 0.750000,
        .TexCoord, 0.625000, 0.750000,
        .TexCoord, 0.375000, 0.750000,
        .TexCoord, 0.625000, 1.000000,
        .TexCoord, 0.375000, 1.000000,
        .TexCoord, 0.375000, 0.000000,
        .TexCoord, 0.625000, 0.000000,
        .TexCoord, 0.625000, 0.250000,
        .TexCoord, 0.375000, 0.250000,
        .TexCoord, 0.125000, 0.500000,
        .TexCoord, 0.375000, 0.500000,
        .TexCoord, 0.125000, 0.750000,

        .Normal, 0.0000, 1.0000, 0.0000,
        .Normal, 0.0000, 0.0000, 1.0000,
        .Normal, -1.0000, 0.0000, 0.0000,
        .Normal, 0.0000, -1.0000, 0.0000,
        .Normal, 1.0000, 0.0000, 0.0000,
        .Normal, 0.0000, 0.0000, -1.0000,

        .UseMat, "Material",
        .Smooth, "off",

        .Face, .{1,1,1},  .{5,2,1},  .{7,3,1},  .{3,4,1},
        .Face, .{4,5,2},  .{3,4,2},  .{7,6,2},  .{8,7,2},
        .Face, .{8,8,3},  .{7,9,3},  .{5,10,3}, .{6,11,3},
        .Face, .{6,12,4}, .{2,13,4}, .{4,5,4},  .{8,14,4},
        .Face, .{2,13,5}, .{1,1,5},  .{3,4,5},  .{4,5,5},
        .Face, .{6,11,6}, .{5,10,6}, .{1,1,6},  .{2,13,6}
    }));

    // make sure data has the correct values

    // Test triangulation of that data
    
    // Test vertex de-duplication
}
