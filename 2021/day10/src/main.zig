const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

fn parse(input: []const u8) !std.ArrayList([]const u8) {
    var ret = std.ArrayList([]const u8).init(allocator);
    var iter = std.mem.splitSequence(u8, input, os_delimiter);
    while (iter.next()) |line| {
        try ret.append(line);
    }
    return ret;
}

fn matching(open: u8, close: u8) bool {
    return switch (open) {
        '(' => close == ')',
        '[' => close == ']',
        '{' => close == '}',
        '<' => close == '>',
        else => unreachable,
    };
}

fn lookup1(c: u8) u64 {
    return switch (c) {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        else => unreachable,
    };
}

fn lookup2(c: u8) u64 {
    return switch (c) {
        '(' => 1,
        '[' => 2,
        '{' => 3,
        '<' => 4,
        else => unreachable,
    };
}

fn lessThan(context: void, a: u64, b: u64) std.math.Order {
    _ = context;
    return std.math.order(a, b);
}

fn part1(input: std.ArrayList([]const u8)) !u64 {
    var ret: u64 = 0;
    for (input.items) |line| {
        var stack = std.ArrayList(u8).init(allocator);
        defer stack.deinit();
        for (line) |c| {
            switch (c) {
                '(', '[', '{', '<' => try stack.append(c),
                ')', ']', '}', '>' => {
                    const last = stack.popOrNull() orelse 'X';
                    if (!matching(last, c)) {
                        ret += lookup1(c);
                    }
                },
                else => unreachable,
            }
        }
    }
    return ret;
}

fn part2(input: std.ArrayList([]const u8)) !u64 {
    var pq = std.PriorityQueue(u64, void, lessThan).init(allocator, {});
    defer pq.deinit();
    for (input.items) |line| {
        var stack = std.ArrayList(u8).init(allocator);
        defer stack.deinit();
        const is_incomplete = for (line) |c| {
            switch (c) {
                '(', '[', '{', '<' => try stack.append(c),
                ')', ']', '}', '>' => {
                    const last = stack.popOrNull() orelse 'X';
                    if (!matching(last, c)) {
                        break false;
                    }
                },
                else => unreachable,
            }
        } else true;
        if (!is_incomplete) {
            continue;
        }
        var score: u64 = 0;
        for (0..stack.items.len) |i| {
            score = (5 * score) + lookup2(stack.items[stack.items.len - 1 - i]);
        }
        try pq.add(score);
    }
    const m = pq.count() / 2;
    for (0..m) |_| {
        _ = pq.remove();
    }
    return pq.remove();
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const input = try parse(content);
    defer input.deinit();
    const result1 = try part1(input);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = try part2(input);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
