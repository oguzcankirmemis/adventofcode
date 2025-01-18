const std = @import("std");
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

const pkgs = struct {
    const common = std.build.Pkg{
        .name = "common",
        .source = .{ .path = "libs/common.zig" },
        .dependencies = &[_]std.build.Pkg{},
    };
};

pub fn build(b: *std.Build) !void {
    const day = b.option(u8, "day", "Day to build") orelse 0;
    const name = try std.fmt.allocPrint(allocator, "day{d}", .{day});
    defer allocator.free(name);
    const root = try std.fmt.allocPrint(allocator, "day{d}/src/main.zig", .{day});
    defer allocator.free(root);
    const default_input = try std.fmt.allocPrint(allocator, "day{d}/inputs/example.txt", .{day});
    defer allocator.free(default_input);
    const input_path = b.option([]const u8, "input", "Input path") orelse default_input;
    const optimize = b.standardOptimizeOption(.{});
    const common = b.createModule(.{
        .root_source_file = b.path("lib/common.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    const exe = b.addExecutable(.{
        .name = name,
        .root_source_file = b.path(root),
        .target = b.graph.host,
        .optimize = optimize,
    });
    exe.root_module.addImport("common", common);
    const options = b.addOptions();
    options.addOption([]const u8, "input_path", input_path);
    exe.root_module.addOptions("config", options);
    b.installArtifact(exe);
    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the solution");
    run_step.dependOn(&run_exe.step);
}
