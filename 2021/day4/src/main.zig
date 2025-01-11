const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const os_delimiter: []const u8 = if (builtin.target.os.tag == .windows) "\r\n" else "\n";
const input_path = config.input_path;

const Bingo = struct {
    sum: u64,
    numbers: [5][5]u64,
    rows_numbers_left: [5]u64,
    cols_numbers_left: [5]u64,
    finished: bool,
};

const Input = struct {
    numbers: std.ArrayList(u64),
    bingos: std.ArrayList(Bingo),
};

fn parseNumbers(input: []const u8) !std.ArrayList(u64) {
    var numbers = std.ArrayList(u64).init(allocator);
    var iter = std.mem.splitScalar(u8, input, ',');
    while (iter.next()) |number| {
        const parsed = try std.fmt.parseUnsigned(u64, number, 10);
        try numbers.append(parsed);
    }
    return numbers;
}

fn parseBingo(input: []const u8) !Bingo {
    var iter = std.mem.splitSequence(u8, input, os_delimiter);
    var c: usize = 0;
    var bingo = Bingo{
        .sum = 0,
        .numbers = undefined,
        .rows_numbers_left = [_]u64{5} ** 5,
        .cols_numbers_left = [_]u64{5} ** 5,
        .finished = false,
    };
    while (iter.next()) |row| {
        for (0..5) |r| {
            var begin: usize = r * 3;
            const end: usize = begin + 2;
            if (row[begin] == ' ') {
                begin += 1;
            }
            const num = try std.fmt.parseUnsigned(u64, row[begin..end], 10);
            bingo.numbers[c][r] = num;
            bingo.sum += num;
        }
        c += 1;
    }
    return bingo;
}

fn parse(input: []const u8) !Input {
    var parsed = Input{
        .numbers = undefined,
        .bingos = std.ArrayList(Bingo).init(allocator),
    };
    var iter = std.mem.splitSequence(u8, input, os_delimiter ** 2);
    const numbers = iter.next().?;
    parsed.numbers = try parseNumbers(numbers);
    while (iter.next()) |bingo| {
        try parsed.bingos.append(try parseBingo(bingo));
    }
    return parsed;
}

fn part1(input: *Input) u64 {
    for (input.numbers.items) |num| {
        for (input.bingos.items) |*bingo| {
            for (0..5) |c| {
                for (0..5) |r| {
                    if (num == bingo.numbers[c][r]) {
                        bingo.sum -= num;
                        bingo.cols_numbers_left[c] -= 1;
                        bingo.rows_numbers_left[r] -= 1;
                        if (bingo.cols_numbers_left[c] == 0 or bingo.rows_numbers_left[r] == 0) {
                            return num * bingo.sum;
                        }
                    }
                }
            }
        }
    }
    return 0;
}

fn part2(input: *Input) u64 {
    var finished_bingos: usize = 0;
    for (input.numbers.items) |num| {
        for (input.bingos.items) |*bingo| {
            if (bingo.finished) {
                continue;
            }
            for (0..5) |c| {
                for (0..5) |r| {
                    if (num == bingo.numbers[c][r]) {
                        bingo.sum -= num;
                        bingo.cols_numbers_left[c] -= 1;
                        bingo.rows_numbers_left[r] -= 1;
                        if (bingo.cols_numbers_left[c] == 0 or bingo.rows_numbers_left[r] == 0) {
                            bingo.finished = true;
                            finished_bingos += 1;
                            if (finished_bingos == input.bingos.items.len) {
                                return num * bingo.sum;
                            }
                        }
                    }
                }
            }
        }
    }
    return 0;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    var input1 = try parse(content);
    defer input1.numbers.deinit();
    defer input1.bingos.deinit();
    var input2 = try parse(content);
    defer input2.numbers.deinit();
    defer input2.bingos.deinit();
    const result1 = part1(&input1);
    try stdout.writer().print("Part 2: {d}\n", .{result1});
    const result2 = part2(&input2);
    try stdout.writer().print("Part 2: {d}", .{result2});
}
