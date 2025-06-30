const std = @import("std");
const img = @import("zimg");

const logger = std.log.scoped(.rsh_lang);

const Allocator = std.mem.Allocator;

const fs = std.fs;
const Dir = fs.Dir;
const File = fs.File;
const Child = std.process.Child;

// Exports

pub const obj = @import("obj_loader.zig");

pub const CompileError = error{
    Unknown,
    ShaderCompilationError,
};

pub const CompileResult = union(enum) {
    Success: []u8,
    Failure: struct {
        status: anyerror = CompileError.Unknown,
        message: ?[]u8,
    },
};

pub const Stage = enum {
    Vertex,
    Fragment,
};

/// ## Brief
/// compiles glsl source code into SPIR-V binaries
///
/// ## Description
/// Takes the given glsl source file and compiles the source code
/// into SPIR-V binaries fit for consumption by compatible graphics APIs,
/// provided that the given file is a valid GLSL source file
///
/// ## Notes
/// * Use the provided ```stage``` parameter to explicitly specify which stage the shader's source code is intended for, otherwise glslc will attempt to infer it (via the file's extension)
/// * Allocates the resultant array using the given allocator
pub fn compileShaderAlloc(filename: []const u8, stage: ?Stage, allocator: Allocator) CompileResult {
    var scratch_arena = std.heap.ArenaAllocator.init(allocator);
    defer scratch_arena.deinit();

    return handleShaderCompilation(
        filename,
        stage,
        scratch_arena.allocator(),
        allocator,
    ) catch |err| sh: {
        break :sh CompileResult{ .Failure = .{
            .status = err,
            .message = null,
        } };
    };
}

pub fn loadImageFile(filename: []const u8, allocator: Allocator) !img.Image {
    return try img.Image.fromFilePath(allocator, filename);
}


/// internal handler function for shader compilation...
/// The public API function helps with setting up the tagged union a bit better
/// this makes error handling less tedious since the only type of error I plan to explicitly handle
/// is an actual glslc compiler error.
///
/// ## Notes
/// Overall, I would say this is a very flaky and gross function,
/// and I am looking forwards to doing a more well-thought out refiniement pass
/// once I actually start fleshing out the asset pipelin
fn handleShaderCompilation(
    source_filename: []const u8,
    stage: ?Stage,
    scratch: Allocator, //dropped at the  end of the function scope
    persistent: Allocator, //persists and is the responsibility of the caller (all outputs are allocated using this)
) !CompileResult {
    const dir = fs.cwd();

    const source_file = try dir.openFile(source_filename, .{});
    source_file.close();

    const output_filename = try std.mem.concat(scratch, u8, &[_][]const u8{
        source_filename,
        ".spv",
    });

    std.debug.print("Output Filename: {s}\n", .{output_filename});

    var compile_process: Child = undefined;

    if (stage != null) {
        const stage_arg = switch (stage.?) {
            .Fragment => "frag",
            .Vertex => "vert",
        };

        const stage_name = "-fshader-stage=" ++ stage_arg;

        compile_process = Child.init(&[_][]const u8{
            "glslc",
            source_filename,
            "-o",
            output_filename,
            stage_name,
        }, scratch);
    } else {
        compile_process = Child.init(&[_][]const u8{
            "glslc",
            source_filename,
            "-o",
            output_filename,
        }, scratch);
    }

    compile_process.stdout_behavior = .Pipe;
    compile_process.stderr_behavior = .Pipe;
    try compile_process.spawn();

    var err_output = try std.ArrayListUnmanaged(u8).initCapacity(scratch, 4096);
    var std_output = try std.ArrayListUnmanaged(u8).initCapacity(scratch, 4096);

    try compile_process.collectOutput(scratch, &std_output, &err_output, 4096);
    const status = try compile_process.wait();

    switch (status) {
        .Exited => |code| {
            std.debug.print("GLSLC process exited with code {d}\n", .{code});

            if (code != 0) {
                return CompileResult{
                    .Failure = .{
                        .status = CompileError.ShaderCompilationError,
                        .message = try std_output.toOwnedSlice(persistent),
                    },
                };
            } else {
                const output_file = try dir.openFile(output_filename, .{});

                // temporary and dangerous!!
                const file_contents = try output_file.readToEndAlloc(persistent, std.math.maxInt(usize));
                return CompileResult{
                    .Success = file_contents,
                };
            }
        },
        else => return error.CompileProcessFailed,
    }
}

// basic tests to make sure compilation works at least most of some of the time
const expect = std.testing.expect;

test "valid sources" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();

    const result = compileShaderAlloc("test/shader.vert", null, allocator);

    try expect(result.Success.len != 0);
}

test "compilation error" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();

    const result = compileShaderAlloc("test/bad_shader.glsl", .Fragment, allocator);

    std.debug.print("Error {s}: \n{d}\n", .{ @errorName(result.Failure.status), result.Failure.message.?.len });

    try expect(result.Failure.status == CompileError.ShaderCompilationError);
}

test "nonexistent file" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();

    const result = compileShaderAlloc("test/nonexistent.glsl", .Fragment, allocator);

    try expect(result.Failure.status == File.OpenError.FileNotFound);
}
