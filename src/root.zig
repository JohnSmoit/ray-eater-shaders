const std = @import("std");
const img = @import("zimg");

const logger = std.log.scoped(.rsh_lang);

const Allocator = std.mem.Allocator;

const fs = std.fs;
const Dir = fs.Dir;
const File = fs.File;
const Child = std.process.Child;

const c = @import("c.zig").c;

// Exports

//pub const obj = @import("obj_loader.zig");
pub fn loadImageFile(filename: []const u8, allocator: Allocator) !img.Image {
    return try img.Image.fromFilePath(allocator, filename);
}

pub const ShaderCompiler = struct {
    const BufferList = std.ArrayListUnmanaged([]const u8);
    const Status = enum { Success, Failure };

    /// # Brief
    /// the result of a shader compilation,
    ///
    /// ## Usage
    /// Represents either a successfully compiled shader binary module, or a
    /// failed compilation with error information.
    /// In the case of allocated error messsages and shader binaries,
    /// their lifetime is bound to the parent ShaderCompiler object
    pub const Result = union(Status) {
        Success: []const u8,
        Failure: struct {
            status: anyerror = Error.Unknown,
            message: ?[]const u8,
        },
    };

    pub const Stage = enum(u8) {
        Vertex = c.shaderc_vertex_shader,
        Fragment = c.shaderc_fragment_shader,
        Compute = c.shaderc_compute_shader,
    };

    pub const Error = error{
        Unknown,
        ShaderCompilationError,
        ShaderCompilerInitFailed,
        CompilerAllocationsFailed,
    };

    allocator: std.mem.Allocator,
    compiler: c.shaderc_compiler_t,

    fn handleShaderComp(
        self: *ShaderCompiler,
        src: []const u8,
        stage: Stage,
        filename: []const u8,
    ) !Result {
        const shader_kind: c_uint = @intFromEnum(stage);
        const res = c.shaderc_compile_into_spv(
            self.compiler,
            src.ptr,
            src.len,
            shader_kind,
            filename.ptr,
            "main",
            null,
        ) orelse
            return Error.CompilerAllocationsFailed;

        const status = c.shaderc_result_get_compilation_status(res);
        if (status != c.shaderc_compilation_status_success) {
            const error_msg: [*:0]const u8 = @ptrCast(c.shaderc_result_get_error_message(res));

            return Result{ .Failure = .{
                .status = Error.ShaderCompilationError,
                .message = std.mem.span(error_msg),
            } };
        }

        const res_size = c.shaderc_result_get_length(res);
        const bytes = c.shaderc_result_get_bytes(res)[0..res_size];

        return Result{
            .Success = bytes,
        };
    }

    pub fn init(allocator: Allocator) !ShaderCompiler {
        return .{
            .allocator = allocator,
            .compiler = c.shaderc_compiler_initialize() orelse
                return error.ShaderCompilerInitFailed,
        };
    }

    pub fn deinit(self: *ShaderCompiler) void {
        c.shaderc_compiler_release(self.compiler);
    }

    /// ## Brief
    /// compiles glsl source code into SPIR-V binaries
    ///
    /// ## Description
    /// Takes the given glsl source file and compiles the source code
    /// into SPIR-V binaries fit for consumption by compatible graphics APIs,
    /// provided that the given file is a valid GLSL source file
    ///
    /// ## Notes
    /// * Use the provided ```stage``` parameter to explicitly specify which stage the shader's source is
    /// * Allocates the resultant array using the given allocator
    pub fn fromFile(
        self: *ShaderCompiler,
        filename: []const u8,
        stage: Stage,
    ) Result {
        // attempt to infer the shader's stage
        var src_arena = std.heap.ArenaAllocator.init(self.allocator);
        defer src_arena.deinit();

        const input_file = std.fs.cwd().openFile(filename, .{
            .mode = .read_only,
        }) catch |e| return Result{
            .Failure = .{
                .message = "file not found",
                .status = e,
            },
        };
        defer input_file.close();
        const file_size: usize = @intCast(input_file.getEndPos() catch 0);

        // FIX: remove FUCK
        const src_buf = src_arena.allocator().alloc(u8, file_size) catch @panic("FUCK");
        _ = input_file.readAll(src_buf) catch {};

        return self.handleShaderComp(src_buf, stage, filename) catch |e| Result{
            .Failure = .{
                .message = null,
                .status = e,
            },
        };
    }

    pub fn fromSrc(
        self: *ShaderCompiler,
        src: []const u8,
        stage: Stage,
    ) Result {
        return self.handleShaderComp(src, stage, "shader.glsl") catch |e| Result{
            .Failure = .{
                .message = null,
                .status = e,
            },
        };
    }
};

// basic tests to make sure compilation works at least most of some of the time
const expect = std.testing.expect;

const CompileResult = ShaderCompiler.Result;
const CompileStatus = ShaderCompiler.Status;
const ShaderError = ShaderCompiler.Error;

fn expectStatus(res: CompileResult, status: CompileStatus) !void {
    switch (res) {
        .Success => {
            if (status != .Success) {
                logger.err("Recieved success instead of failure: {s}", .{res.Success});
                return error.InvalidStatus;
            }
        },
        .Failure => {
            if (status != .Failure) {
                logger.err(
                    "Recieved failure instead of success: \nmsg: {s}\nstatus: {!}",
                    .{
                        res.Failure.message orelse "(no message)",
                        res.Failure.status,
                    },
                );
                return error.InvalidStatus;
            }
        }
    }
}

fn expectAlignment(res: CompileResult, by: u32) !void {
    if (!switch (res) {
        .Failure => false,
        .Success => |bytes| sb: {
            if (@rem(bytes.len, by) != 0 or @rem(@intFromPtr(bytes.ptr), by) != 0) {
                logger.err("Invalid aligmnent: {d} extra bytes", .{@rem(bytes.len, by)});
                break :sb false;
            }

            break :sb true;
        },
    }) return error.InvalidAlignment;
}

const TestingAllocator = std.heap.DebugAllocator(.{
    .safety = true,
});

test "valid sources" {
    var arena = TestingAllocator.init;
    const allocator = arena.allocator();

    var compiler = try ShaderCompiler.init(allocator);
    defer compiler.deinit();

    const result = compiler.fromFile("test/shader.vert", .Vertex);

    try expectStatus(result, .Success);
    try expect(result.Success.len != 0);
    try expectAlignment(result, 4);
    try expect(arena.deinit() == .ok);
}

test "compilation error" {
    var arena = TestingAllocator.init;
    const allocator = arena.allocator();

    var compiler = try ShaderCompiler.init(allocator);
    defer compiler.deinit();

    const result = compiler.fromSrc("test/bad_shader.glsl", .Fragment);

    try expectStatus(result, .Failure);
    try expect(result.Failure.status == ShaderError.ShaderCompilationError);
    try expect(arena.deinit() == .ok);
}

test "nonexistent file" {
    var arena = TestingAllocator.init;
    const allocator = arena.allocator();

    var compiler = try ShaderCompiler.init(allocator);
    defer compiler.deinit();

    const result = compiler.fromFile("test/nonexistent.glsl", .Fragment);

    try expectStatus(result, .Failure);
    try expect(result.Failure.status == File.OpenError.FileNotFound);
    try expect(arena.deinit() == .ok);
}

test "multi complations" {
    var arena = TestingAllocator.init;
    const allocator = arena.allocator();

    var compiler = try ShaderCompiler.init(allocator);
    defer compiler.deinit();

    for (0..2) |_| {
        const result = compiler.fromFile("test/shader.vert", .Vertex);

        try expectStatus(result, .Success);
        try expect(result.Success.len != 0);
        try expectAlignment(result, 4);
    }
    try expect(arena.deinit() == .ok);
}

test "inferrence" {}

test "compute" {
    var arena = TestingAllocator.init;
    const allocator = arena.allocator();

    var compiler = try ShaderCompiler.init(allocator);
    defer compiler.deinit();

    const result = compiler.fromFile("test/compute.glsl", .Compute);

    try expectStatus(result, .Success);
    try expect(result.Success.len != 0);
    try expectAlignment(result, 4);
    try expect(arena.deinit() == .ok);
}
