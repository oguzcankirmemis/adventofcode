const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");
const common = @import("common");

const MAX_FILE_SIZE: usize = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;

const Area = struct {
    min: common.Vec2,
    max: common.Vec2,
};

const SolutionInterval = struct {
    in: u64,
    out: u64,
    is_infinite: bool,
    no_solution: bool,
};

fn calculatePositionX(vel: i64, timestep: u64) i64 {
    const i64_timestep: i64 = @intCast(timestep);
    const max = if (vel < 0) -@divFloor(vel * (vel - 1), 2) else @divFloor(vel * (vel + 1), 2);
    const absolute_vel = if (vel < 0) -vel else vel;
    if (timestep > absolute_vel) {
        return max;
    }
    const minus = if (vel < 0) -@divFloor(((vel + i64_timestep) * (vel + i64_timestep - 1)), 2) else @divFloor((vel - i64_timestep) * (vel - i64_timestep + 1), 2);
    return max - minus;
}

fn leftmost_binary_search(min: u64, max: u64, target: i64, vel: i64, calculatePosition: fn (vel: i64, timestep: u64) i64) u64 {
    var left = min;
    var right = max + 1;
    while (left < right) {
        const middle = (left + right) / 2;
        const pos = calculatePosition(vel, middle);
        if (pos < target) {
            left = middle + 1;
        } else {
            right = middle;
        }
    }
    return left;
}

fn rightmost_binary_search(min: u64, max: u64, target: i64, vel: i64, calculatePosition: fn (vel: i64, timestep: u64) i64) u64 {
    var left = min;
    var right = max + 1;
    while (left < right) {
        const middle = (left + right) / 2;
        const pos = calculatePosition(vel, middle);
        if (pos > target) {
            right = middle;
        } else {
            left = middle + 1;
        }
    }
    return right - 1;
}

fn calculateTimestepsInAreaX(area: Area, x_vel: i64) SolutionInterval {
    var ret = SolutionInterval{
        .in = 0,
        .out = 0,
        .is_infinite = false,
        .no_solution = true,
    };
    ret.in = leftmost_binary_search(0, @intCast(x_vel), area.min.x, x_vel, calculatePositionX);
    ret.out = rightmost_binary_search(0, @intCast(x_vel), area.max.x, x_vel, calculatePositionX);
    const pos = calculatePositionX(x_vel, ret.in);
    if (pos < area.min.x or pos > area.max.x) {
        return ret;
    }
    ret.no_solution = false;
    if (ret.out == x_vel) {
        ret.is_infinite = true;
    }
    if (ret.out < ret.in) {
        ret.out = ret.in;
    }
    return ret;
}

fn getHashMapX(area: Area) !std.AutoArrayHashMap(u64, SolutionInterval) {
    var ret = std.AutoArrayHashMap(u64, SolutionInterval).init(allocator);
    const u64_max_x: u64 = @intCast(area.max.x);
    for (0..(u64_max_x + 1)) |x_vel| {
        const interval = calculateTimestepsInAreaX(area, @intCast(x_vel));
        if (interval.no_solution) {
            continue;
        }
        try ret.put(x_vel, interval);
    }
    return ret;
}

fn abs(x: i64) u64 {
    if (x < 0) {
        return @intCast(-x);
    }
    return @intCast(x);
}

fn getSolutions(area: Area, hash_map_x: std.AutoArrayHashMap(u64, SolutionInterval), y_vel: i64) !u64 {
    var ret: u64 = 0;
    var found_solutions = std.AutoArrayHashMap(u64, bool).init(allocator);
    defer found_solutions.deinit();
    var y_pos = area.min.y;
    while (y_pos <= area.max.y) : (y_pos += 1) {
        const discriminant: i64 = ((y_vel * 2) + 1) * ((y_vel * 2) + 1) - 8 * y_pos;
        if (discriminant < 0) {
            continue;
        }
        const sqrt = std.math.sqrt(@as(u64, @intCast(discriminant)));
        if (sqrt * sqrt != discriminant) {
            continue;
        }
        const timestep1 = ((y_vel * 2) + 1) + sqrt;
        const timestep2 = ((y_vel * 2) + 1) - sqrt;
        var u64_timestep1: u64 = 0;
        var u64_timestep2: u64 = 0;
        if (timestep1 >= 0 and @mod(timestep1, 2) == 0) {
            u64_timestep1 = @intCast(@divExact(timestep1, 2));
        }
        if (timestep2 >= 0 and @mod(timestep2, 2) == 0) {
            u64_timestep2 = @intCast(@divExact(timestep2, 2));
        }
        var iterator = hash_map_x.iterator();
        while (iterator.next()) |entry| {
            if (found_solutions.contains(entry.key_ptr.*)) {
                continue;
            }
            var isSolution = false;
            if (u64_timestep1 >= entry.value_ptr.in and u64_timestep1 <= entry.value_ptr.out) {
                isSolution = true;
            }
            if (u64_timestep1 > entry.value_ptr.out and entry.value_ptr.is_infinite) {
                isSolution = true;
            }
            if (u64_timestep2 >= entry.value_ptr.in and u64_timestep2 <= entry.value_ptr.out) {
                isSolution = true;
            }
            if (u64_timestep2 > entry.value_ptr.out and entry.value_ptr.is_infinite) {
                isSolution = true;
            }
            if (isSolution) {
                try found_solutions.put(entry.key_ptr.*, true);
                ret += 1;
            }
        }
    }
    return ret;
}

fn part1(area: Area) !u64 {
    var hash_map_x = try getHashMapX(area);
    defer hash_map_x.deinit();
    const max_y_vel = if (abs(area.min.y) > abs(area.max.y)) abs(area.min.y) else abs(area.max.y);
    var ret: u64 = 0;
    for (0..(max_y_vel + 1)) |y_vel| {
        const solutions = try getSolutions(area, hash_map_x, @intCast(y_vel));
        if (solutions > 0) {
            ret = y_vel;
        }
    }
    return (ret * (ret + 1)) / 2;
}

fn part2(area: Area) !u64 {
    var hash_map_x = try getHashMapX(area);
    defer hash_map_x.deinit();
    var max_timestep: u64 = 0;
    var iterator = hash_map_x.iterator();
    while (iterator.next()) |entry| {
        if (entry.key_ptr.* > max_timestep) {
            max_timestep = entry.key_ptr.*;
        }
    }
    const max_y_vel: i64 = @intCast(if (abs(area.min.y) > abs(area.max.y)) abs(area.min.y) else abs(area.max.y));
    const min_y_vel: i64 = area.min.y;
    var ret: u64 = 0;
    var y_vel = min_y_vel;
    while (y_vel <= max_y_vel) : (y_vel += 1) {
        ret += try getSolutions(area, hash_map_x, @intCast(y_vel));
    }
    return ret;
}

fn parse(input: []const u8) !Area {
    var iterator = std.mem.splitSequence(u8, input, ", ");
    const x = iterator.next().?;
    const y = iterator.next().?;
    var x_iterator = std.mem.splitSequence(u8, x, "x=");
    var y_iterator = std.mem.splitSequence(u8, y, "y=");
    _ = x_iterator.next().?;
    _ = y_iterator.next().?;
    const x_interval = x_iterator.next().?;
    const y_interval = y_iterator.next().?;
    var x_interval_iterator = std.mem.splitSequence(u8, x_interval, "..");
    var y_interval_iterator = std.mem.splitSequence(u8, y_interval, "..");
    return Area{
        .min = common.Vec2{
            .x = try std.fmt.parseInt(i64, x_interval_iterator.next().?, 10),
            .y = try std.fmt.parseInt(i64, y_interval_iterator.next().?, 10),
        },
        .max = common.Vec2{
            .x = try std.fmt.parseInt(i64, x_interval_iterator.next().?, 10),
            .y = try std.fmt.parseInt(i64, y_interval_iterator.next().?, 10),
        },
    };
}

pub fn main() !void {
    defer {
        if (gpa.deinit() == .leak) {
            std.debug.print("Memory leak detected!\n", .{});
        }
    }
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const target_area = try parse(content);
    const result1 = try part1(target_area);
    try writer.print("Part 1: {d}\n", .{result1});
    const result2 = try part2(target_area);
    try writer.print("Part 2: {d}\n", .{result2});
}
