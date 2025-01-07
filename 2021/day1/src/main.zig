const std = @import("std");

const MAX_FILE_SIZE: usize = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

fn parse(input: []u8) !std.ArrayList(i64) {
    var ret = std.ArrayList(i64).init(allocator);
    var iter = std.mem.splitSequence(u8, input, "\n");
    while (iter.next()) |number| {
        const parsed = try std.fmt.parseInt(i64, std.mem.trim(u8, number, "\r"), 10);
        try ret.append(parsed);
    }
    return ret;
}

fn part1(numbers: std.ArrayList(i64)) u64 {
    var count: u64 = 0;
    for (numbers.items[1..], 1..) |number, i| {
        if (number > numbers.items[i - 1]) {
            count += 1;
        }
    }
    return count;
}

fn part2(numbers: std.ArrayList(i64)) u64 {
    var count: u64 = 0;
    for (numbers.items[3..], 3..) |number, i| {
        var total: i64 = 0;
        for (0..4) |dec| {
            total += numbers.items[i - dec];
        }
        const prev_window = total - number;
        const next_window = total - numbers.items[i - 3];
        if (next_window > prev_window) {
            count += 1;
        }
    }
    return count;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile("day1/inputs/real.txt", .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    const numbers = try parse(content);
    defer numbers.deinit();
    const result1 = part1(numbers);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = part2(numbers);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
