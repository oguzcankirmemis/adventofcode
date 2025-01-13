const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE: usize = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.target.os.tag == .windows) "\r\n" else "\n";

const Display = struct {
    digits: [10][7]bool,
    output: [4][7]bool,
};

fn findOne(display: Display) [7]bool {
    for (display.digits) |digit| {
        var count: usize = 0;
        for (digit) |b| {
            count += if (b) 1 else 0;
        }
        if (count == 2) {
            return digit;
        }
    }
    unreachable;
}

fn findTwo(display: Display, a: usize, d: usize, e: usize, g: usize) [7]bool {
    for (display.digits) |digit| {
        var count: usize = 0;
        for (digit) |b| {
            count += if (b) 1 else 0;
        }
        if (count == 5 and digit[a] and digit[d] and digit[e] and digit[g]) {
            return digit;
        }
    }
    unreachable;
}

fn findThree(display: Display, one: [7]bool) [7]bool {
    for (display.digits) |digit| {
        var count: usize = 0;
        for (digit) |b| {
            count += if (b) 1 else 0;
        }
        if (count == 5) {
            var matches = true;
            for (0..7) |i| {
                if (one[i] and !digit[i]) {
                    matches = false;
                    break;
                }
            }
            if (matches) {
                return digit;
            }
        }
    }
    unreachable;
}

fn findFour(display: Display) [7]bool {
    for (display.digits) |digit| {
        var count: usize = 0;
        for (digit) |b| {
            count += if (b) 1 else 0;
        }
        if (count == 4) {
            return digit;
        }
    }
    unreachable;
}

fn findSeven(display: Display) [7]bool {
    for (display.digits) |digit| {
        var count: usize = 0;
        for (digit) |b| {
            count += if (b) 1 else 0;
        }
        if (count == 3) {
            return digit;
        }
    }
    unreachable;
}

fn findNine(display: Display, four: [7]bool) [7]bool {
    for (display.digits) |digit| {
        var count: usize = 0;
        for (digit) |b| {
            count += if (b) 1 else 0;
        }
        if (count == 6) {
            var matches = true;
            for (0..7) |i| {
                if (four[i] and !digit[i]) {
                    matches = false;
                    break;
                }
            }
            if (matches) {
                return digit;
            }
        }
    }
    unreachable;
}

fn findA(one: [7]bool, seven: [7]bool) usize {
    for (0..7) |i| {
        if (one[i] != seven[i]) {
            return i;
        }
    }
    unreachable;
}

fn findG(four: [7]bool, nine: [7]bool, a: usize) usize {
    for (0..7) |i| {
        if (!four[i] and i != a and nine[i]) {
            return i;
        }
    }
    unreachable;
}

fn findD(one: [7]bool, three: [7]bool, a: usize, g: usize) usize {
    for (0..7) |i| {
        if (i != a and i != g and !one[i] and three[i]) {
            return i;
        }
    }
    unreachable;
}

fn findE(four: [7]bool, a: usize, d: usize, g: usize) usize {
    for (0..7) |i| {
        if (i != a and i != d and i != g and !four[i]) {
            return i;
        }
    }
    unreachable;
}

fn findB(one: [7]bool, nine: [7]bool, a: usize, d: usize, g: usize) usize {
    for (0..7) |i| {
        if (i != a and i != d and i != g and !one[i] and nine[i]) {
            return i;
        }
    }
    unreachable;
}

fn findC(two: [7]bool, a: usize, d: usize, e: usize, g: usize) usize {
    for (0..7) |i| {
        if (i != a and i != d and i != e and i != g and two[i]) {
            return i;
        }
    }
    unreachable;
}

fn findF(one: [7]bool, c: usize) usize {
    for (0..7) |i| {
        if (i != c and one[i]) {
            return i;
        }
    }
    unreachable;
}

fn solveDisplay(display: Display) [7]usize {
    var ret: [7]usize = undefined;
    const one = findOne(display);
    const three = findThree(display, one);
    const four = findFour(display);
    const seven = findSeven(display);
    const nine = findNine(display, four);
    ret[0] = findA(one, seven);
    ret[6] = findG(four, nine, ret[0]);
    ret[3] = findD(one, three, ret[0], ret[6]);
    ret[4] = findE(four, ret[0], ret[3], ret[6]);
    const two = findTwo(display, ret[0], ret[3], ret[4], ret[6]);
    ret[1] = findB(one, nine, ret[0], ret[3], ret[6]);
    ret[2] = findC(two, ret[0], ret[3], ret[4], ret[6]);
    ret[5] = findF(one, ret[2]);
    return ret;
}

fn parseDisplayDigit(solution: [7]usize, digit: [7]bool) u64 {
    var count: usize = 0;
    for (digit) |b| {
        count += if (b) 1 else 0;
    }
    if (count == 2) {
        return 1;
    }
    if (count == 3) {
        return 7;
    }
    if (count == 4) {
        return 4;
    }
    if (count == 5) {
        if (digit[solution[2]] and digit[solution[4]]) {
            return 2;
        }
        if (digit[solution[1]] and digit[solution[5]]) {
            return 5;
        }
        if (digit[solution[2]] and digit[solution[5]]) {
            return 3;
        }
    }
    if (count == 6) {
        if (!digit[solution[3]]) {
            return 0;
        }
        if (!digit[solution[2]]) {
            return 6;
        }
        if (!digit[solution[4]]) {
            return 9;
        }
    }
    if (count == 7) {
        return 8;
    }
    unreachable;
}

fn parseDigit(input: []const u8) [7]bool {
    var ret: [7]bool = [_]bool{false} ** 7;
    const base: u8 = 'a';
    for (input) |char| {
        ret[char - base] = true;
    }
    return ret;
}

fn parse(input: []const u8) !std.ArrayList(Display) {
    var ret = std.ArrayList(Display).init(allocator);
    var line_iter = std.mem.splitSequence(u8, input, os_delimiter);
    while (line_iter.next()) |line| {
        var display = Display{
            .digits = undefined,
            .output = undefined,
        };
        var pipe_iter = std.mem.splitSequence(u8, line, " | ");
        const digits = pipe_iter.next().?;
        var digits_iter = std.mem.splitScalar(u8, digits, ' ');
        var i: usize = 0;
        while (digits_iter.next()) |digit| : (i += 1) {
            display.digits[i] = parseDigit(digit);
        }
        const output = pipe_iter.next().?;
        var output_iter = std.mem.splitScalar(u8, output, ' ');
        i = 0;
        while (output_iter.next()) |digit| : (i += 1) {
            display.output[i] = parseDigit(digit);
        }
        try ret.append(display);
    }
    return ret;
}

fn part1(input: std.ArrayList(Display)) usize {
    var count: usize = 0;
    for (input.items) |display| {
        const solution = solveDisplay(display);
        for (display.output) |digit| {
            const d = parseDisplayDigit(solution, digit);
            if (d == 1 or d == 4 or d == 7 or d == 8) {
                count += 1;
            }
        }
    }
    return count;
}

fn part2(input: std.ArrayList(Display)) u64 {
    var ret: u64 = 0;
    for (input.items) |display| {
        const solution = solveDisplay(display);
        var output: u64 = 0;
        for (display.output) |digit| {
            const d = parseDisplayDigit(solution, digit);
            output = (10 * output) + d;
        }
        ret += output;
    }
    return ret;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const input = try parse(content);
    defer input.deinit();
    const result1 = part1(input);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = part2(input);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
