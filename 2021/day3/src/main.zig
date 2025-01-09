const std = @import("std");
const config = @import("config");

const MAX_FILE_SIZE: usize = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;

fn parse(input: []u8) !std.ArrayList([]const u8) {
    var ret = std.ArrayList([]const u8).init(allocator);
    var iter = std.mem.splitSequence(u8, input, "\n");
    while (iter.next()) |number| {
        const trimmed = std.mem.trim(u8, number, "\r");
        try ret.append(trimmed);
    }
    return ret;
}

fn oneCounts(numbers: [][]const u8, buffer: []u64) []const u64 {
    const number_length = numbers[0].len;
    var one_counts = buffer[0..number_length];
    for (0..number_length) |i| {
        one_counts[i] = 0;
    }
    for (numbers) |number| {
        for (number, 0..) |bit, i| {
            one_counts[i] += if (bit == '1') 1 else 0;
        }
    }
    return one_counts;
}

fn part1(numbers: std.ArrayList([]const u8), one_counts: []const u64) u64 {
    var epsilon: u64 = 0;
    var gamma: u64 = 0;
    for (one_counts) |count| {
        epsilon *= 2;
        gamma *= 2;
        if (count > numbers.items.len - count) {
            gamma += 1;
        } else {
            epsilon += 1;
        }
    }
    return epsilon * gamma;
}

fn oxygen(numbers: std.ArrayList([]const u8), one_counts: []const u64) !u64 {
    var prev = try allocator.alloc([]const u8, numbers.items.len);
    defer allocator.free(prev);
    var next = try allocator.alloc([]const u8, numbers.items.len);
    defer allocator.free(next);
    var buffer: [128]u64 = [_]u64{0} ** 128;
    var current: [][]const u8 = numbers.items;
    var current_one_counts = one_counts;
    var filtered_numbers_length = numbers.items.len;
    var bit_index: usize = 0;
    while (filtered_numbers_length != 1) : (filtered_numbers_length = current.len) {
        var new_filtered_numbers_length: usize = 0;
        for (current) |number| {
            if (current_one_counts[bit_index] >= current.len - current_one_counts[bit_index] and number[bit_index] == '1') {
                next[new_filtered_numbers_length] = number;
                new_filtered_numbers_length += 1;
            }
            if (current_one_counts[bit_index] < current.len - current_one_counts[bit_index] and number[bit_index] == '0') {
                next[new_filtered_numbers_length] = number;
                new_filtered_numbers_length += 1;
            }
        }
        current = next[0..new_filtered_numbers_length];
        current_one_counts = oneCounts(current, &buffer);
        const tmp = prev;
        prev = next;
        next = tmp;
        bit_index += 1;
    }
    return try std.fmt.parseUnsigned(u64, current[0], 2);
}

fn co2(numbers: std.ArrayList([]const u8), one_counts: []const u64) !u64 {
    var prev = try allocator.alloc([]const u8, numbers.items.len);
    defer allocator.free(prev);
    var next = try allocator.alloc([]const u8, numbers.items.len);
    defer allocator.free(next);
    var buffer: [128]u64 = [_]u64{0} ** 128;
    var current: [][]const u8 = numbers.items;
    var current_one_counts = one_counts;
    var filtered_numbers_length = numbers.items.len;
    var bit_index: usize = 0;
    while (filtered_numbers_length != 1) : (filtered_numbers_length = current.len) {
        var new_filtered_numbers_length: usize = 0;
        for (current) |number| {
            if (current_one_counts[bit_index] >= current.len - current_one_counts[bit_index] and number[bit_index] == '0') {
                next[new_filtered_numbers_length] = number;
                new_filtered_numbers_length += 1;
            }
            if (current_one_counts[bit_index] < current.len - current_one_counts[bit_index] and number[bit_index] == '1') {
                next[new_filtered_numbers_length] = number;
                new_filtered_numbers_length += 1;
            }
        }
        current = next[0..new_filtered_numbers_length];
        current_one_counts = oneCounts(current, &buffer);
        const tmp = prev;
        prev = next;
        next = tmp;
        bit_index += 1;
    }
    return try std.fmt.parseUnsigned(u64, current[0], 2);
}

fn part2(numbers: std.ArrayList([]const u8), one_counts: []const u64) !u64 {
    const oxygen_level = try oxygen(numbers, one_counts);
    const co2_level = try co2(numbers, one_counts);
    return oxygen_level * co2_level;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const numbers = try parse(content);
    defer numbers.deinit();
    var buffer: [128]u64 = [_]u64{0} ** 128;
    const one_counts = oneCounts(numbers.items, &buffer);
    const result1 = part1(numbers, one_counts);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = try part2(numbers, one_counts);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
