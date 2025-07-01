const std = @import("std");

const AnyReader = std.io.AnyReader;
const Allocator = std.mem.Allocator; 
const ArenaAllocator = std.heap.ArenaAllocator;

const File = std.fs.File;

const Self = @This();



const DirectiveType = enum(u8) {
    Vertex,
    TexCoord,
    Normal,
    Face,
    Name,
    UseMat,
    MatLib,
    Smooth,
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
        .{"s", .Smooth},
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
            .String => std.mem.eql(u8, self.String, other.String),
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

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .Directive => |d| try writer.print("Directive: {s}", .{@tagName(d)}),
            .String => |s| try writer.print("String: {s}", .{s}),
            .FloatingPoint => |f| try writer.print("Float: {d}", .{f}),
            .FaceDef => |fd| try writer.print("Face: {any}", .{fd}),
            .EOF =>  try writer.print("EOF", .{}),
        }


        _ = fmt;
        _ = options;
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
            if (part.len ==  0) {
                faces[ind] = 0;
                continue;
            }

            faces[ind] = std.fmt.parseInt(u32, part, 0) catch return null;
        }

        return faces;

    }

    pub fn fromRaw(raw: []const u8, allocator: Allocator) !Token {
        if (dir_map.get(raw)) |v| {
            return Token{.Directive = v};
        }

        const face: ?[3]u32 = parseFaceDef(raw);
        if (face) |f| {
            return Token{.FaceDef = f};
        }

        const fp: ?f32 = std.fmt.parseFloat(f32, raw) catch null;
        if (fp) |v| {
            return Token{.FloatingPoint = v};
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

pub fn fromFilePath(path: []const u8, allocator: Allocator) !Self {
    const file = try std.fs.cwd().openFile(path, .{.mode = .read_only});
    defer file.close();

    return try fromFile(file, allocator);
}

pub fn fromFile(file: File, allocator: Allocator) !Self {

    var loader = Self{
        .allocator = allocator,
        .str_allocator = ArenaAllocator.init(allocator),
        .tokens = try TokenList.initCapacity(allocator, 64),
    };

    try loader.tokenize(file);

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


    fn isDelimiter(c: u8) bool {
        const delim = [_]u8{' ', '\t', '\r', '\n'};

        for (delim) |d| {
            if (c == d) return true;
        }

        return false;
    }

    pub fn nextLine(self: *TokenizerState) ?[]const u8 {
        var line_begin = self.pos;
        var line_end = self.pos;

        while (self.buf[self.pos] != '\n') : (self.pos += 1) {
            if (self.pos == self.buf_size) {
                const len = line_end - line_begin;
                const fpos = self.file.getPos() catch return null;

                self.file.seekTo(fpos - len) catch return null;
                self.buf_size = self.file.read(self.buf) catch return null;
                
                self.pos = 0;
                line_begin = 0;
                line_end = len;

                if (self.buf_size == 0) return null;
            }

            line_end += 1;
        }

        self.pos += 1;

        return self.buf[line_begin..line_end];
    }

    /// ## Notes:
    /// - A return value of null indicates the EOF token...
    pub fn nextTokenRaw(line_pos: usize, line: []const u8) ?[]const u8 {
        // Stop conditions: 
        //  - Whitespace character encountered
        //  - End of File encountered
        //      - reach buf size position, re read, then buf size = 0
        //
        // Special Case: 
        //  - End of buffer in the middle of token
        //      - Handle this by refilling the buffer at an offset and resetting the token indices
        //  - Returned token slice is valid until the next token is retrieved, since the buffer
        //    could refill then

        // eat preceding whitespace
        if (line.len == 0 or line_pos >= line.len) return null;

        var pos = line_pos;

        while (pos != line.len and isDelimiter(line[pos])) : (pos += 1) {}

        const sym_begin = pos;
        var sym_end = pos;


        while (pos != line.len and !isDelimiter(line[pos])) : (pos += 1) {
            sym_end += 1;
        }

        if (sym_end == sym_begin) {
            std.debug.print("End of the line bro\n", .{});
            return null;
        }

        return line[sym_begin..sym_end];
    }
};


fn tokenize(self: *Self, file: File) !void {

    // Do tokenization stuff
    const allocator = self.str_allocator.allocator();
    const buf = try allocator.alloc(u8, buf_size);
    defer self.allocator.free(buf);

    var tokenizer = TokenizerState{
        .buf = buf,
        .file = file,
    };

    tokenizer.buf_size = try file.read(buf);
    while (tokenizer.nextLine()) |line| {
        var line_pos: usize = 0;

        if (line.len > 0 and line[0] == '#') continue;

        while (TokenizerState.nextTokenRaw(line_pos, line)) |raw| {
            const tok = try Token.fromRaw(raw, allocator);
            try self.tokens.append(allocator, tok);

            line_pos += raw.len + 1;
        }

    }

    try self.tokens.append(allocator, .EOF);
}

// =====================================================
// *********** ANALYSIS AND OUTPUT CREATION ************
// =====================================================

pub const Vec3f = struct {
    x: f32,
    y: f32,
    z: f32,
};

pub const LoadResult = union(enum) {
    Failure: struct {
        err: anyerror,
        token: ?*const Token = null,
    },

    // For simple models, we can just ignore shape groups and merge it all together
    Success: struct {
        verts: []const Vec3f,
        indices: []const u32,

        allocator: Allocator,

        pub fn deinit(self: *@This()) void {
            self.allocator.free(self.verts);
            self.allocator.free(self.indices);
        }
    },
};

pub const TriangulationMode = enum {
    Full, // just include all vertices
    DiscardByNormals, // discards duplicate vertices (by excluding normals)
};

// ====================================
// ********* DIRECTIVE TYPES **********
// ====================================

const VertRef = struct {
    x: *const f32,
    y: *const f32,
    z: *const f32,
};

const TexRef = struct {
    u: *const f32,
    v: *const f32,
};

const NormRef = struct {
    x: *const f32,
    y: *const f32,
    z: *const f32,
};

const FaceRef = struct {
    faces: [4]*const [3]u32,
};

const Table = struct {

    verts: std.ArrayList(VertRef),
    uvs: std.ArrayList(TexRef),
    normals: std.ArrayList(NormRef),
    faces: std.ArrayList(FaceRef),

    // FIXME: Partial allocation failures result in the lists being created previously
    // memory leaking all over the place
    pub fn init(allocator: Allocator) !@This() {
        return .{
            .verts = try std.ArrayList(VertRef).init(allocator),
            .uvs = try std.ArrayList(TexRef).init(allocator),
            .normals = try std.ArrayList(NormRef).init(allocator),
            .faces = try std.ArrayList(FaceRef).init(allocator),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.verts.deinit();
        self.uvs.deinit();
        self.normals.deinit();
        self.faces.deinit();
    }
};

/// iterator-style next token function
/// ## Notes:
/// * Upon encountering the EOF token, returns null
pub fn nextToken(self: *Self) ?*const Token {
}

/// All of these directive parsers return null on success
/// otherwise, they return the invalid token index in question
fn parseVertex(self: *Self, tbl: *Table) ?usize {

} 

fn parseTexCoord(self: *Self, tbl: *Table) ?usize {

}

fn parseNormal(self: *Self, tbl: *Table) ?usize {

}

fn parseFace(self: *Self, tbl: *Table) ?usize {

}

// ## Notes:
// * All output data is allocated since we don't really know how much of it there'll be at comptime
// * I don't use the instance bound allocator for output since, I want the user to own the resulting resource
pub fn getData(self: *Self, tri: TriangulationMode, allocator: Allocator) LoadResult {
    var table = Table.init(self.allocator) catch |err| return .{.Failure = .{.err = err}};

    while (self.nextToken()) |tok| {
        if (tok.getType() != .Directive) {
            return .{.Failure = .{.err = error.DirectiveExpected, .token = tok}}; 
        }

        // token dispatch table
        const res = switch (tok.Directive) {
            .Vertex => self.parseVertex(&table),
            .TexCoord => self.parseTexCoord(&table),
            .Normal => self.parseNormal(&table),
            .Face => self.parseFace(&table),
            else => self.skip(),
        };

        if (res != null) {
            return .{.Failure = .{.err = error.InvalidToken, .token = res.?}};
        }
    }
    _ = tri;
}

// ===================================
// *********** UNIT TESTS ************
// ===================================


const log = std.log.scoped(.obj_loader);

fn validateAgainstTokens(self: *const Self, tokens: []const Token) !void {
    for (self.tokens.items, 0..) |tok, index| {
        if (!tok.eql(tokens[index])) {
            log.err("Token Mismatch: \nExpected: {s} \nVS \nGot: {s}", .{tokens[index], tok});
            return error.MismatchedToken;
        }
    }
}

fn expectEqual(tok_str: []const u8, b: Token, allocator: Allocator) !void {
    const tok = try Token.fromRaw(tok_str, allocator);
    if (!tok.eql(b)) {
        log.err("Token Mismatch\nExpected: {s} \nGot: {s}\n", .{b, tok});
    }
}


fn makeTokens(comptime values: anytype) []const Token {
    comptime var tokens: []const Token = &.{};

    inline for (values) |val| {
        tokens = tokens ++ [_]Token{
            switch(@TypeOf(val)) {
                DirectiveType => Token{.Directive = val},

                comptime_float => Token{.FloatingPoint = val},
                
                [3]comptime_int => Token{.FaceDef = [3]u32{val[0], val[1], val[2]}},

                []const u8 => Token{.String = val},

                else => {
                    @compileLog("Bad: ", val);
                    @compileError("Invalid token");
                },
            }
        };
    }

    tokens = tokens ++ [_]Token{.EOF};

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
    var data: Self = try Self.fromFilePath("test/cube.obj", allocator);
    try data.validateAgainstTokens(makeTokens(.{
        DirectiveType.MatLib, @as([]const u8, "cube.mtl"), // These should just coerce automatically tbh like why
        DirectiveType.Name, @as([]const u8, "Cube"),
        DirectiveType.Vertex, 1.000000, 1.000000, -1.000000,
        DirectiveType.Vertex, 1.000000, -1.000000, -1.000000,
        DirectiveType.Vertex, 1.000000, 1.000000, 1.000000,
        DirectiveType.Vertex, 1.000000, -1.000000, 1.000000,
        DirectiveType.Vertex, -1.000000, 1.000000, -1.000000,
        DirectiveType.Vertex, -1.000000, -1.000000, -1.000000,
        DirectiveType.Vertex, -1.000000, 1.000000, 1.000000,
        DirectiveType.Vertex, -1.000000, -1.000000,1.000000,

        DirectiveType.TexCoord, 0.625000, 0.500000,
        DirectiveType.TexCoord, 0.875000, 0.500000,
        DirectiveType.TexCoord, 0.875000, 0.750000,
        DirectiveType.TexCoord, 0.625000, 0.750000,
        DirectiveType.TexCoord, 0.375000, 0.750000,
        DirectiveType.TexCoord, 0.625000, 1.000000,
        DirectiveType.TexCoord, 0.375000, 1.000000,
        DirectiveType.TexCoord, 0.375000, 0.000000,
        DirectiveType.TexCoord, 0.625000, 0.000000,
        DirectiveType.TexCoord, 0.625000, 0.250000,
        DirectiveType.TexCoord, 0.375000, 0.250000,
        DirectiveType.TexCoord, 0.125000, 0.500000,
        DirectiveType.TexCoord, 0.375000, 0.500000,
        DirectiveType.TexCoord, 0.125000, 0.750000,

        DirectiveType.Normal, 0.0000, 1.0000, 0.0000,
        DirectiveType.Normal, 0.0000, 0.0000, 1.0000,
        DirectiveType.Normal, -1.0000, 0.0000, 0.0000,
        DirectiveType.Normal, 0.0000, -1.0000, 0.0000,
        DirectiveType.Normal, 1.0000, 0.0000, 0.0000,
        DirectiveType.Normal, 0.0000, 0.0000, -1.0000,

        DirectiveType.UseMat, @as([]const u8, "Material"),
        DirectiveType.Smooth, @as([]const u8, "off"),

        DirectiveType.Face, [_]comptime_int{1,1,1},  [_]comptime_int{5,2,1},  [_]comptime_int{7,3,1},  [_]comptime_int{3,4,1},
        DirectiveType.Face, [_]comptime_int{4,5,2},  [_]comptime_int{3,4,2},  [_]comptime_int{7,6,2},  [_]comptime_int{8,7,2},
        DirectiveType.Face, [_]comptime_int{8,8,3},  [_]comptime_int{7,9,3},  [_]comptime_int{5,10,3}, [_]comptime_int{6,11,3},
        DirectiveType.Face, [_]comptime_int{6,12,4}, [_]comptime_int{2,13,4}, [_]comptime_int{4,5,4},  [_]comptime_int{8,14,4},
        DirectiveType.Face, [_]comptime_int{2,13,5}, [_]comptime_int{1,1,5},  [_]comptime_int{3,4,5},  [_]comptime_int{4,5,5},
        DirectiveType.Face, [_]comptime_int{6,11,6}, [_]comptime_int{5,10,6}, [_]comptime_int{1,1,6},  [_]comptime_int{2,13,6}
    }));

    // make sure data has the correct values

    // Test triangulation of that data
    
    // Test vertex de-duplication
}
