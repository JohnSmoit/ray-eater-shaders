const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const img_mod = b.dependency("zigimg", .{}).module("zigimg");

    const mod = b.addModule("rshc", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize, 
    });

    mod.addImport("zimg", img_mod);

    const lib = b.addLibrary(.{
        .name = "rshc",
        .root_module = mod,
        .linkage = .static,
    });

    b.installArtifact(lib);

    const tests = b.addTest(.{
        .root_module = mod,
    });

    const test_run_cmd = b.addRunArtifact(tests);
    const test_step = b.step("test-all", "run all rshc unit tests");
    test_step.dependOn(&test_run_cmd.step);
}
