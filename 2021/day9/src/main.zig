const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.target.os.tag == .windows) "\r\n" else "\n";

fn parse(input: []const u8) !std.ArrayList(std.ArrayList(u64)) {
    var ret = std.ArrayList(std.ArrayList(u64)).init(allocator);
    var iter = std.mem.splitSequence(u8, input, os_delimiter);
    while (iter.next()) |line| {
        var row = std.ArrayList(u64).init(allocator);
        for (line) |num| {
            try row.append(num - '0');
        }
        try ret.append(row);
    }
    return ret;
}

fn lowPoints(height_map: std.ArrayList(std.ArrayList(u64))) !std.ArrayList([2]u64) {
    var ret = std.ArrayList([2]u64).init(allocator);
    for (height_map.items, 0..) |row, i| {
        for (row.items, 0..) |height, j| {
            var low_point = true;
            if (j > 0 and height_map.items[i].items[j - 1] <= height) {
                low_point = false;
            }
            if (j < height_map.items[i].items.len - 1 and height_map.items[i].items[j + 1] <= height) {
                low_point = false;
            }
            if (i > 0 and height_map.items[i - 1].items[j] <= height) {
                low_point = false;
            }
            if (i < height_map.items.len - 1 and height_map.items[i + 1].items[j] <= height) {
                low_point = false;
            }
            if (low_point) {
                try ret.append([2]u64{ i, j });
            }
        }
    }
    return ret;
}

fn dfs(height_map: std.ArrayList(std.ArrayList(u64)), visited: std.ArrayList(std.ArrayList(bool)), i: usize, j: usize) u64 {
    const h = height_map.items[i].items[j];
    visited.items[i].items[j] = true;
    if (h == 9) {
        return 0;
    }
    var n: u64 = 1;
    if (i > 0 and height_map.items[i - 1].items[j] > h and !visited.items[i - 1].items[j]) {
        n += dfs(height_map, visited, i - 1, j);
    }
    if (i < height_map.items.len - 1 and height_map.items[i + 1].items[j] > h and !visited.items[i + 1].items[j]) {
        n += dfs(height_map, visited, i + 1, j);
    }
    if (j > 0 and height_map.items[i].items[j - 1] > h and !visited.items[i].items[j - 1]) {
        n += dfs(height_map, visited, i, j - 1);
    }
    if (j < height_map.items[i].items.len - 1 and height_map.items[i].items[j + 1] > h and !visited.items[i].items[j + 1]) {
        n += dfs(height_map, visited, i, j + 1);
    }
    return n;
}

fn part1(height_map: std.ArrayList(std.ArrayList(u64))) u64 {
    var ret: u64 = 0;
    for (height_map.items, 0..) |row, i| {
        for (row.items, 0..) |height, j| {
            var low_point = true;
            if (j > 0 and height_map.items[i].items[j - 1] <= height) {
                low_point = false;
            }
            if (j < height_map.items[i].items.len - 1 and height_map.items[i].items[j + 1] <= height) {
                low_point = false;
            }
            if (i > 0 and height_map.items[i - 1].items[j] <= height) {
                low_point = false;
            }
            if (i < height_map.items.len - 1 and height_map.items[i + 1].items[j] <= height) {
                low_point = false;
            }
            if (low_point) {
                ret += height + 1;
            }
        }
    }
    return ret;
}

fn part2(height_map: std.ArrayList(std.ArrayList(u64))) !u64 {
    const low_points = try lowPoints(height_map);
    defer low_points.deinit();
    var max1: u64 = 0;
    var max2: u64 = 0;
    var max3: u64 = 0;
    var visited = std.ArrayList(std.ArrayList(bool)).init(allocator);
    defer {
        for (visited.items) |row| {
            row.deinit();
        }
        visited.deinit();
    }
    for (height_map.items) |row| {
        var r = std.ArrayList(bool).init(allocator);
        for (row.items) |_| {
            try r.append(false);
        }
        try visited.append(r);
    }
    for (low_points.items) |point| {
        const i = point[0];
        const j = point[1];
        const c = dfs(height_map, visited, i, j);
        if (c > max1) {
            max3 = max2;
            max2 = max1;
            max1 = c;
        } else if (c > max2) {
            max3 = max2;
            max2 = c;
        } else if (c > max3) {
            max3 = c;
        }
    }
    return max1 * max2 * max3;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const height_map = try parse(content);
    defer {
        for (height_map.items) |row| {
            row.deinit();
        }
        height_map.deinit();
    }
    const result1 = part1(height_map);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = try part2(height_map);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
