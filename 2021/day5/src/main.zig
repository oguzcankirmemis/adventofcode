const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const os_delimiter: []const u8 = if (builtin.target.os.tag == .windows) "\r\n" else "\n";
const input_path = config.input_path;

const Vec2 = struct {
    x: i64,
    y: i64,

    const Vec2KeyContext = struct {
        pub fn hash(_: Vec2KeyContext, key: Vec2) u64 {
            const p1: i64 = 73856093;
            const p2: i64 = 19349663;
            return @intCast((key.x * p1) ^ (key.y * p2));
        }

        pub fn eql(_: Vec2KeyContext, a: Vec2, b: Vec2) bool {
            return a.x == b.x and a.y == b.y;
        }
    };
};

const Line = struct {
    start: Vec2,
    end: Vec2,
};

fn parsePoint(input: []const u8) !Vec2 {
    var iter = std.mem.splitScalar(u8, input, ',');
    const x = iter.next().?;
    const y = iter.next().?;
    return Vec2{
        .x = try std.fmt.parseInt(i64, x, 10),
        .y = try std.fmt.parseInt(i64, y, 10),
    };
}

fn parseLine(input: []const u8) !Line {
    var iter = std.mem.splitSequence(u8, input, " -> ");
    const start = iter.next().?;
    const end = iter.next().?;
    return Line{
        .start = try parsePoint(start),
        .end = try parsePoint(end),
    };
}

fn parse(input: []const u8) !std.ArrayList(Line) {
    var lines = std.ArrayList(Line).init(allocator);
    var iter = std.mem.splitSequence(u8, input, os_delimiter);
    while (iter.next()) |line| {
        try lines.append(try parseLine(line));
    }
    return lines;
}

fn part1(lines: std.ArrayList(Line)) !u64 {
    var count: u64 = 0;
    var map = std.HashMap(Vec2, u64, Vec2.Vec2KeyContext, std.hash_map.default_max_load_percentage).init(allocator);
    defer map.deinit();
    for (lines.items) |line| {
        if (line.start.x != line.end.x and line.start.y != line.end.y) {
            continue;
        }
        const dx = std.math.sign(line.end.x - line.start.x);
        const dy = std.math.sign(line.end.y - line.start.y);
        const stop_x = line.end.x + dx;
        const stop_y = line.end.y + dy;
        var x = line.start.x;
        var y = line.start.y;
        while (x != stop_x or y != stop_y) : ({
            x += dx;
            y += dy;
        }) {
            const entry = try map.getOrPut(.{ .x = x, .y = y });
            if (entry.found_existing) {
                entry.value_ptr.* += 1;
            } else {
                entry.value_ptr.* = 1;
            }
            if (entry.value_ptr.* == 2) {
                count += 1;
            }
        }
    }
    return count;
}

fn part2(lines: std.ArrayList(Line)) !u64 {
    var count: u64 = 0;
    var map = std.HashMap(Vec2, u64, Vec2.Vec2KeyContext, std.hash_map.default_max_load_percentage).init(allocator);
    defer map.deinit();
    for (lines.items) |line| {
        const dx = std.math.sign(line.end.x - line.start.x);
        const dy = std.math.sign(line.end.y - line.start.y);
        const stop_x = line.end.x + dx;
        const stop_y = line.end.y + dy;
        var x = line.start.x;
        var y = line.start.y;
        while (x != stop_x or y != stop_y) : ({
            x += dx;
            y += dy;
        }) {
            const entry = try map.getOrPut(.{ .x = x, .y = y });
            if (entry.found_existing) {
                entry.value_ptr.* += 1;
            } else {
                entry.value_ptr.* = 1;
            }
            if (entry.value_ptr.* == 2) {
                count += 1;
            }
        }
    }
    return count;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const lines = try parse(content);
    defer lines.deinit();
    const result1 = try part1(lines);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = try part2(lines);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
