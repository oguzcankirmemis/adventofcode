const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");
const common = @import("common");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const Vec2 = common.Vec2;
const Vec3 = common.Vec3;
const HashsetType = std.HashMap(Vec3, bool, Vec3.Vec3KeyContext, std.hash_map.default_max_load_percentage);
const Cuboid = struct {
    on: bool,
    x: Vec2,
    y: Vec2,
    z: Vec2,

    fn parseRange(input: []const u8) !Vec2 {
        var iter = std.mem.splitSequence(u8, input[2..], "..");
        const low = try std.fmt.parseInt(i64, iter.next().?, 10);
        const high = try std.fmt.parseInt(i64, iter.next().?, 10);
        return Vec2{ .x = low, .y = high };
    }

    fn parse(input: []const u8, on: bool) !Cuboid {
        var iter = std.mem.splitScalar(u8, input, ',');
        const x = try parseRange(iter.next().?);
        const y = try parseRange(iter.next().?);
        const z = try parseRange(iter.next().?);
        return Cuboid{ .on = on, .x = x, .y = y, .z = z };
    }
};

const Collision = struct {
    x: Vec2,
    y: Vec2,
    z: Vec2,
};

const StackElement = struct {
    c: Cuboid,
    i: usize,
};

const Result = struct {
    part1: usize,
    part2: usize,
};

fn parse(input: []const u8) !std.ArrayList(Cuboid) {
    var ret = std.ArrayList(Cuboid).init(allocator);
    var iter = std.mem.splitSequence(u8, input, os_delimiter);
    while (iter.next()) |cuboid_str| {
        var cuboid_iter = std.mem.splitScalar(u8, cuboid_str, ' ');
        const on = cuboid_iter.next().?.len == 2;
        const cuboid = cuboid_iter.next().?;
        try ret.append(try Cuboid.parse(cuboid, on));
    }
    return ret;
}

fn solve_part1_naive(input: *const std.ArrayList(Cuboid)) !u64 {
    var set = HashsetType.init(allocator);
    defer set.deinit();
    for (input.items) |cuboid| {
        std.debug.print("{}\n", .{cuboid});
        const x_low = if (cuboid.x.x < -50) -50 else cuboid.x.x;
        const x_high = if (cuboid.x.y > 50) 50 else cuboid.x.y;
        const y_low = if (cuboid.y.x < -50) -50 else cuboid.y.x;
        const y_high = if (cuboid.y.y > 50) 50 else cuboid.y.y;
        const z_low = if (cuboid.z.x < -50) -50 else cuboid.z.x;
        const z_high = if (cuboid.z.y > 50) 50 else cuboid.z.y;
        var x = x_low;
        while (x <= x_high) : (x += 1) {
            var y = y_low;
            while (y <= y_high) : (y += 1) {
                var z = z_low;
                while (z <= z_high) : (z += 1) {
                    if (cuboid.on) {
                        try set.put(Vec3{ .x = x, .y = y, .z = z }, true);
                    } else {
                        _ = set.remove(Vec3{ .x = x, .y = y, .z = z });
                    }
                }
            }
        }
    }
    return set.count();
}

fn interval_collision(v1: Vec2, v2: Vec2) ?Vec2 {
    if (v1.y < v2.x or v2.y < v1.x) {
        return null;
    }
    if (v1.x >= v2.x) {
        return Vec2{
            .x = v1.x,
            .y = if (v1.y <= v2.y) v1.y else v2.y,
        };
    }
    return Vec2{
        .x = v2.x,
        .y = if (v2.y <= v1.y) v2.y else v1.y,
    };
}

fn cuboid_collision(c1: Cuboid, c2: Cuboid) ?Collision {
    const xCollision = interval_collision(c1.x, c2.x);
    if (xCollision == null) {
        return null;
    }
    const yCollision = interval_collision(c1.y, c2.y);
    if (yCollision == null) {
        return null;
    }
    const zCollision = interval_collision(c1.z, c2.z);
    if (zCollision == null) {
        return null;
    }
    return Collision{
        .x = xCollision.?,
        .y = yCollision.?,
        .z = zCollision.?,
    };
}

fn subtract_collision(c: Cuboid, col: Collision) [6]?Cuboid {
    var t = c;
    var ret = [6]?Cuboid{ null, null, null, null, null, null };
    var i: usize = 0;
    if (t.x.x < col.x.x) {
        ret[i] = Cuboid{
            .on = t.on,
            .x = Vec2{ .x = t.x.x, .y = col.x.x - 1 },
            .y = Vec2{ .x = t.y.x, .y = t.y.y },
            .z = Vec2{ .x = t.z.x, .y = t.z.y },
        };
        t.x = Vec2{ .x = col.x.x, .y = t.x.y };
        i += 1;
    }
    if (t.x.y > col.x.y) {
        ret[i] = Cuboid{
            .on = t.on,
            .x = Vec2{ .x = col.x.y + 1, .y = t.x.y },
            .y = Vec2{ .x = t.y.x, .y = t.y.y },
            .z = Vec2{ .x = t.z.x, .y = t.z.y },
        };
        t.x = Vec2{ .x = t.x.x, .y = col.x.y };
        i += 1;
    }
    if (t.y.x < col.y.x) {
        ret[i] = Cuboid{
            .on = t.on,
            .x = Vec2{ .x = t.x.x, .y = t.x.y },
            .y = Vec2{ .x = t.y.x, .y = col.y.x - 1 },
            .z = Vec2{ .x = t.z.x, .y = t.z.y },
        };
        t.y = Vec2{ .x = col.y.x, .y = t.y.y };
        i += 1;
    }
    if (t.y.y > col.y.y) {
        ret[i] = Cuboid{
            .on = t.on,
            .x = Vec2{ .x = t.x.x, .y = t.x.y },
            .y = Vec2{ .x = col.y.y + 1, .y = t.y.y },
            .z = Vec2{ .x = t.z.x, .y = t.z.y },
        };
        t.y = Vec2{ .x = t.y.x, .y = col.y.y };
        i += 1;
    }
    if (t.z.x < col.z.x) {
        ret[i] = Cuboid{
            .on = t.on,
            .x = Vec2{ .x = t.x.x, .y = t.x.y },
            .y = Vec2{ .x = t.y.x, .y = t.y.y },
            .z = Vec2{ .x = t.z.x, .y = col.z.x - 1 },
        };
        t.z = Vec2{ .x = col.z.x, .y = t.z.y };
        i += 1;
    }
    if (t.z.y > col.z.y) {
        ret[i] = Cuboid{
            .on = t.on,
            .x = Vec2{ .x = t.x.x, .y = t.x.y },
            .y = Vec2{ .x = t.y.x, .y = t.y.y },
            .z = Vec2{ .x = col.z.y + 1, .y = t.z.y },
        };
        t.z = Vec2{ .x = t.z.x, .y = col.z.y };
        i += 1;
    }
    return ret;
}

fn remove_offs(instructions: *const std.ArrayList(Cuboid)) !std.ArrayList(Cuboid) {
    var ret = std.ArrayList(Cuboid).init(allocator);
    for (instructions.items) |c| {
        if (c.on) {
            try ret.append(c);
        } else {
            const len = ret.items.len;
            var i: usize = 0;
            while (i < len) {
                const o = ret.items[i];
                if (cuboid_collision(c, o)) |col| {
                    for (subtract_collision(o, col)) |r| {
                        if (r == null) {
                            break;
                        }
                        try ret.append(r.?);
                    }
                    _ = ret.swapRemove(i);
                } else {
                    i += 1;
                }
            }
        }
    }
    return ret;
}

fn add_cuboid_without_intersection(cuboids: *std.ArrayList(Cuboid), c: Cuboid) !void {
    var stack = std.ArrayList(StackElement).init(allocator);
    defer stack.deinit();
    try stack.append(StackElement{ .c = c, .i = 0 });
    const len = cuboids.items.len;
    while (stack.items.len > 0) {
        //std.debug.print("Stack: {d}\n", .{stack.items.len});
        const to_add = stack.pop();
        var can_add = true;
        var i = to_add.i;
        while (i < len) : (i += 1) {
            const o = cuboids.items[i];
            if (cuboid_collision(to_add.c, o)) |col| {
                for (subtract_collision(to_add.c, col)) |r| {
                    if (r == null) {
                        break;
                    }
                    try stack.append(StackElement{ .c = r.?, .i = i + 1 });
                }
                can_add = false;
                break;
            }
        }
        if (can_add) {
            try cuboids.append(to_add.c);
        }
    }
}

fn remove_intersections(cuboids: std.ArrayList(Cuboid)) !std.ArrayList(Cuboid) {
    var ret = std.ArrayList(Cuboid).init(allocator);
    for (cuboids.items) |c| {
        try add_cuboid_without_intersection(&ret, c);
    }
    return ret;
}

fn solve(instructions: *const std.ArrayList(Cuboid)) !Result {
    var without_offs = try remove_offs(instructions);
    defer without_offs.deinit();
    var without_intersections = try remove_intersections(without_offs);
    defer without_intersections.deinit();
    var ret = Result{
        .part1 = 0,
        .part2 = 0,
    };
    for (without_intersections.items) |c| {
        var t = c;
        t.on = !(t.x.x > 50 or t.x.y < -50 or
            t.y.x > 50 or t.y.y < -50 or
            t.z.x > 50 or t.z.y < -50);
        if (t.x.x < -50) {
            t.x.x = -50;
        }
        if (t.x.y > 50) {
            t.x.y = 50;
        }
        if (t.y.x < -50) {
            t.y.x = -50;
        }
        if (t.y.y > 50) {
            t.y.y = 50;
        }
        if (t.z.x < -50) {
            t.z.x = -50;
        }
        if (t.z.y > 50) {
            t.z.y = 50;
        }
        if (t.on) {
            ret.part1 += @intCast((t.x.y - t.x.x + 1) * (t.y.y - t.y.x + 1) * (t.z.y - t.z.x + 1));
        }
        ret.part2 += @intCast((c.x.y - c.x.x + 1) * (c.y.y - c.y.x + 1) * (c.z.y - c.z.x + 1));
    }
    return ret;
}

pub fn main() !void {
    const writer = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const input = try parse(content);
    defer input.deinit();
    const result = try solve(&input);
    try writer.print("Part 1: {d}\n", .{result.part1});
    try writer.print("Part 2: {d}\n", .{result.part2});
}
