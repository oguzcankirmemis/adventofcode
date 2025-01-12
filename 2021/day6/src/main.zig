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

fn table() [257][9]u64 {
    var ret: [257][9]u64 = undefined;
    for (0..9) |i| {
        ret[0][i] = 1;
    }
    for (1..257) |i| {
        for (0..9) |j| {
            if (j == 0) {
                ret[i][j] = ret[i - 1][6] + ret[i - 1][8];
            } else {
                ret[i][j] = ret[i - 1][j - 1];
            }
        }
    }
    return ret;
}

fn part1(lanternfish_list: std.ArrayList(u64), dp_table: [257][9]u64) u64 {
    var ret: u64 = 0;
    for (lanternfish_list.items) |i| {
        ret += dp_table[80][i];
    }
    return ret;
}

fn part2(lanternfish_list: std.ArrayList(u64), dp_table: [257][9]u64) u64 {
    var ret: u64 = 0;
    for (lanternfish_list.items) |i| {
        ret += dp_table[256][i];
    }
    return ret;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    const dp_table = table();
    var lanternfish_list = try parse(content);
    defer lanternfish_list.deinit();
    const result1 = part1(lanternfish_list, dp_table);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = part2(lanternfish_list, dp_table);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
