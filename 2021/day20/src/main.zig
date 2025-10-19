const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");
const common = @import("common");

const Vec2 = common.Vec2;
const HashsetType = std.HashMap(Vec2, bool, Vec2.Vec2KeyContext, std.hash_map.default_max_load_percentage);

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimeter = if (builtin.os.tag == .windows) "\r\n" else "n";

const Input = struct {
    algorithm: []const u8,
    non_trivial_pixels: HashsetType,
};

fn parse(input: []const u8) !Input {
    var iter = std.mem.splitSequence(u8, input, os_delimeter ** 2);
    const algorithm = iter.next().?;
    const image_str = iter.next().?;
    var image_iter = std.mem.splitSequence(u8, image_str, os_delimeter);
    var i: i64 = 0;
    var non_trivial_pixels = HashsetType.init(allocator);
    while (image_iter.next()) |row| {
        for (0..row.len) |j| {
            if (row[j] == '#') {
                try non_trivial_pixels.put(Vec2{ .y = i, .x = @intCast(j) }, true);
            }
        }
        i += 1;
    }
    return Input{ .algorithm = algorithm, .non_trivial_pixels = non_trivial_pixels };
}

fn is_one_in_next_iteration(input: *const Input, pixel: *const Vec2, pixel_is_one: bool) bool {
    var index: usize = 0;
    var i: i64 = -1;
    while (i <= 1) : (i += 1) {
        var j: i64 = -1;
        while (j <= 1) : (j += 1) {
            const key = Vec2{ .x = pixel.x + j, .y = pixel.y + i };
            const contains = input.non_trivial_pixels.contains(key);
            index += if (contains and pixel_is_one) 1 else if (!contains and !pixel_is_one) 1 else 0;
            index = index << 1;
        }
    }
    index = index >> 1;
    return input.algorithm[index] == '#';
}

fn iterate(input: *Input, result: *HashsetType, cache: *HashsetType, is_odd_iteration: bool) !void {
    cache.clearRetainingCapacity();
    var iterator = input.non_trivial_pixels.iterator();
    while (iterator.next()) |entry| {
        const pixel = entry.key_ptr;
        var i: i64 = -2;
        while (i <= 2) : (i += 1) {
            var j: i64 = -2;
            while (j <= 2) : (j += 1) {
                const to_compute = Vec2{ .x = pixel.x + j, .y = pixel.y + i };
                if (cache.contains(to_compute)) {
                    continue;
                }
                const is_one = is_one_in_next_iteration(input, &to_compute, is_odd_iteration);
                if (is_one and !is_odd_iteration) {
                    try result.put(to_compute, true);
                }
                if (!is_one and is_odd_iteration) {
                    try result.put(to_compute, true);
                }
                try cache.put(to_compute, true);
            }
        }
    }
}

fn simulate(input: *Input, iterations: usize, start_at_odd_iteration: bool) !void {
    var cache = HashsetType.init(allocator);
    defer cache.deinit();
    var result = HashsetType.init(allocator);
    defer result.deinit();
    var is_odd_iteration = start_at_odd_iteration;
    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        try iterate(input, &result, &cache, is_odd_iteration);
        const tmp = result;
        result = input.non_trivial_pixels;
        input.non_trivial_pixels = tmp;
        is_odd_iteration = !is_odd_iteration;
        result.clearRetainingCapacity();
    }
}

fn solve_part1(input: *Input) !usize {
    try simulate(input, 2, true);
    return input.non_trivial_pixels.count();
}

fn solve_part2(input: *Input) !usize {
    try simulate(input, 48, true);
    return input.non_trivial_pixels.count();
}

pub fn main() !void {
    const writer = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    var input = try parse(content);
    defer input.non_trivial_pixels.deinit();
    const result1 = try solve_part1(&input);
    try writer.print("Part 1: {d}\n", .{result1});
    const result2 = try solve_part2(&input);
    try writer.print("Part 2: {d}\n", .{result2});
}
