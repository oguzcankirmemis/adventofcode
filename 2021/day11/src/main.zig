const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");
const common = @import("common");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.target.os.tag == .windows) "\r\n" else "\n";

fn parse(input: []const u8) !std.ArrayList(std.ArrayList(u8)) {
    var ret = std.ArrayList(std.ArrayList(u8)).init(allocator);
    var iterator = std.mem.splitSequence(u8, input, os_delimiter);
    while (iterator.next()) |line| {
        var row = std.ArrayList(u8).init(allocator);
        for (line) |c| {
            try row.append(c - '0');
        }
        try ret.append(row);
    }
    return ret;
}

fn step(map: std.ArrayList(std.ArrayList(u8))) !u64 {
    var stack = std.ArrayList(common.Index2D).init(allocator);
    defer stack.deinit();
    var reset = std.ArrayList(common.Index2D).init(allocator);
    defer reset.deinit();
    for (map.items, 0..) |row, c| {
        for (row.items, 0..) |*octopus, r| {
            octopus.* += 1;
            if (octopus.* == 10) {
                try stack.append(.{
                    .col = c,
                    .row = r,
                });
            }
        }
    }
    var blinks: u64 = 0;
    while (stack.items.len > 0) {
        var buffer: [8]common.Index2D = undefined;
        const coord = stack.pop();
        try reset.append(coord);
        for (coord.neighbours(map.items.len, map.items[0].items.len, &buffer)) |neighbour| {
            map.items[neighbour.col].items[neighbour.row] += 1;
            if (map.items[neighbour.col].items[neighbour.row] == 10) {
                try stack.append(neighbour);
            }
        }
        blinks += 1;
    }
    for (reset.items) |coord| {
        map.items[coord.col].items[coord.row] = 0;
    }
    return blinks;
}

fn part1(map: std.ArrayList(std.ArrayList(u8))) !u64 {
    var ret: u64 = 0;
    for (0..100) |_| {
        ret += try step(map);
    }
    return ret;
}

fn part2(map: std.ArrayList(std.ArrayList(u8))) !u64 {
    const total = map.items.len * map.items[0].items.len;
    var i: u64 = 1;
    while (try step(map) != total) {
        i += 1;
    }
    return i;
}

pub fn main() !void {
    defer {
        const check = gpa.deinit();
        if (check != .ok) {
            std.debug.print("Memory leakage detected!\n", .{});
        }
    }
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const map1 = try parse(content);
    defer {
        for (map1.items) |row| {
            row.deinit();
        }
        map1.deinit();
    }
    const map2 = try parse(content);
    defer {
        for (map2.items) |row| {
            row.deinit();
        }
        map2.deinit();
    }
    const result1 = try part1(map1);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = try part2(map2);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
