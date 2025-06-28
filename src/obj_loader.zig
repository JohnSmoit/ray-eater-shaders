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
    Directive: DirectiveType,
    String: []const u8,
    FloatingPoint: f32,
    FaceDef: [3]u32,
    EOF,

    pub fn eql(self: Token, other: Token) bool {

    }

    pub fn getType(self: Token) TokenType {
        return switch (self) {
            .Directive => .Directive,
            .String => .String,
            .FloatingPoint => .FloatingPoint,
            .FaceDef => .FaceDef,
        };
    }
};

const TokenList = std.ArrayListUnmanaged(Token);

const buf_size: usize = 1024;

fn makeTokens(comptime values: anytype) []const Token {

}

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


const delim = [_]u8{' ', '\n', '\r', '\t'};

fn tokenize(self: *Self, reader: AnyReader) ObjLoadErrors!void {
    if (self.tokens != null) return;
    
    // Do tokenization stuf
    const buf = try self.allocator.alloc(u8, buf_size);
    defer self.allocator.free(buf);

    var buf_pos: usize = 0;
    var valid_bytes: usize = try reader.read(buf);

    var sym_begin: usize = 0;
    var sym_end: usize = 0;

    // For file
        // For token
    // (Buffer stuff should not be the controlling iterator here)

}



fn validateAgainstTokens(self: *const Self, tokens: []const Token) !void {

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
