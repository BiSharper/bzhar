const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lzss_mod = b.addModule("lzss", .{
        .root_source_file = b.path("src/lzss/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const fileutil_mod = b.addModule("fileutil", .{
        .root_source_file = b.path("src/util/file.zig"),
        .target = target,
        .optimize = optimize,
    });

    const mempools_mod = b.addModule("mempools", .{
        .root_source_file = b.path("src/pools/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const param_mod = b.addModule("param", .{
        .root_source_file = b.path("src/param/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    param_mod.addImport("fileutil", fileutil_mod);
    param_mod.addImport("mempools", mempools_mod);

    // CLI executable
    const cli_exe = b.addExecutable(.{
        .name = "bzhar",
        .root_source_file = b.path("src/cli.zig"),
        .target = target,
        .optimize = optimize,
    });
    cli_exe.root_module.addImport("param", param_mod);
    cli_exe.root_module.addImport("mempools", mempools_mod);
    cli_exe.root_module.addImport("fileutil", fileutil_mod);

    b.installArtifact(cli_exe);

    const lib = b.addSharedLibrary(.{
        .name = "bzhar",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .version = .{ .major = 0, .minor = 0, .patch = 1 },
    });

    lib.root_module.addImport("lzss", lzss_mod);
    lib.root_module.addImport("mempools", mempools_mod);
    lib.root_module.addImport("fileutil", fileutil_mod);
    lib.root_module.addImport("param", param_mod);

    b.installArtifact(lib);

    // Module-scoped unit tests
    const test_step = b.step("test", "Run unit tests (by module)");
    const test_module = b.option([]const u8, "test-module", "Module to test (lzss|mempools|param|root)") orelse "";

    const lzss_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/lzss/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_lzss_unit_tests = b.addRunArtifact(lzss_unit_tests);

    const mempools_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/pools/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    mempools_unit_tests.root_module.addImport("mempools", mempools_mod);
    const run_mempools_unit_tests = b.addRunArtifact(mempools_unit_tests);

    const param_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/param/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    param_unit_tests.root_module.addImport("mempools", mempools_mod);
    param_unit_tests.root_module.addImport("fileutil", fileutil_mod);

    const run_param_unit_tests = b.addRunArtifact(param_unit_tests);

    const root_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    root_unit_tests.root_module.addImport("lzss", lzss_mod);
    root_unit_tests.root_module.addImport("mempools", mempools_mod);
    root_unit_tests.root_module.addImport("param", param_mod);
    const run_root_unit_tests = b.addRunArtifact(root_unit_tests);

    if (test_module.len == 0) {
        test_step.dependOn(&run_lzss_unit_tests.step);
        test_step.dependOn(&run_mempools_unit_tests.step);
        test_step.dependOn(&run_param_unit_tests.step);
        test_step.dependOn(&run_root_unit_tests.step);
    } else if (std.mem.eql(u8, test_module, "lzss")) {
        test_step.dependOn(&run_lzss_unit_tests.step);
    } else if (std.mem.eql(u8, test_module, "mempools")) {
        test_step.dependOn(&run_mempools_unit_tests.step);
    } else if (std.mem.eql(u8, test_module, "param")) {
        test_step.dependOn(&run_param_unit_tests.step);
    } else if (std.mem.eql(u8, test_module, "root")) {
        test_step.dependOn(&run_root_unit_tests.step);
    }
}
