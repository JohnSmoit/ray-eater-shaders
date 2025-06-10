const std = @import("std");

const logger = std.log.scoped(.rsh_lang);

const fs = std.fs;
const Dir = fs.Dir;
const File = fs.File;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    // skip the exe name
    if (!args.skip()) {
        logger.err("No arguments specified -- terminating", .{});
        return error.MissingArguments;
    }

    const source_filename = args.next() orelse {
        logger.err("No source file specified -- terminating", .{});
        return error.MissingSourceFile;
    };

    const output_filename = args.next() orelse {
        logger.err("No output file specified -- terminating", .{});
        return error.MissingOutputFile;
    };

    if (args.skip()) {
        logger.info("Additional arguments specified -- ignoring", .{});
    }

    const input_file = fs.cwd().openFile(source_filename, File.OpenFlags{
        .mode = .read_only,
    }) catch |err| {
        logger.err("Could not open {s} for reading {!} -- terminating", .{ source_filename, err });
        return error.FileAccessDenied;
    };
    input_file.close();

    var compile_process = std.process.Child.init(&[_][]const u8{
        "glslc",
        source_filename,
        "-o",
        output_filename,
    }, allocator);

    logger.info("Compiling {s} to SPIR-V (output: {s})", .{ source_filename, output_filename });

    compile_process.spawn() catch |err| {
        logger.err("Failed to spawn glslc process: {!}", .{err});
        return err;
    };

    const status = try compile_process.wait();

    switch (status) {
        .Exited => |code| {
            if (code != 0) {
                logger.err("glslc process exited with value: {d}", .{code});
                return error.ChildProcessFailed;
            }
        },
        else => {
            logger.err("glslc process {s}'d the f up man!", .{@tagName(status)});
            return error.Fuck;
        },
    }
}
