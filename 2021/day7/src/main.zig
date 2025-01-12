const std = @import("std");
const config = @import("config");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;

fn parse(input: []const u8) !std.ArrayList(u64) {
    var ret = std.ArrayList(u64).init(allocator);
    var iter = std.mem.splitScalar(u8, input, ',');
    while (iter.next()) |num| {
        try ret.append(try std.fmt.parseUnsigned(u64, num, 10));
    }
    return ret;
}

fn part1(crabs: std.ArrayList(u64)) u64 {
    var ret: u64 = std.math.maxInt(u64);
    for (crabs.items) |c1| {
        var curr: u64 = 0;
        for (crabs.items) |c2| {
            curr += if (c1 > c2) c1 - c2 else c2 - c1;
        }
        ret = if (curr < ret) curr else ret;
    }
    return ret;
}

fn part2(crabs: std.ArrayList(u64)) u64 {
    var max: u64 = 0;
    var min: u64 = std.math.maxInt(u64);
    for (crabs.items) |c| {
        max = if (c > max) c else max;
        min = if (c < min) c else min;
    }
    var ret: u64 = std.math.maxInt(u64);
    for (min..(max + 1)) |t| {
        var curr: u64 = 0;
        for (crabs.items) |c| {
            const n = if (t > c) t - c else c - t;
            curr += (n * (n + 1)) / 2;
        }
        ret = if (curr < ret) curr else ret;
    }
    return ret;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const crabs = try parse(content);
    defer crabs.deinit();
    const result1 = part1(crabs);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = part2(crabs);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
