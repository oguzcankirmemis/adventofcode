const std = @import("std");
const config = @import("config");

const MAX_FILE_SIZE: usize = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;

const Vec2 = struct { x: i64, y: i64 };
const Vec3 = struct { x: i64, y: i64, z: i64 };

const Direction = enum {
    forward,
    up,
    down,

    fn move1(self: Direction, amount: u32, position: *Vec2) void {
        switch (self) {
            Direction.forward => position.x += amount,
            Direction.up => position.y -= amount,
            Direction.down => position.y += amount,
        }
    }

    fn move2(self: Direction, amount: u32, position: *Vec3) void {
        switch (self) {
            Direction.forward => {
                position.x += amount;
                position.y += position.z * amount;
            },
            Direction.up => position.z -= amount,
            Direction.down => position.z += amount,
        }
    }

    fn parse(input: []const u8) Direction {
        if (std.mem.eql(u8, input, "forward")) {
            return Direction.forward;
        } else if (std.mem.eql(u8, input, "up")) {
            return Direction.up;
        } else {
            return Direction.down;
        }
    }
};

fn parse(input: []u8) !std.ArrayList(struct { Direction, u32 }) {
    var ret = std.ArrayList(struct { Direction, u32 }).init(allocator);
    var iter = std.mem.splitSequence(u8, input, "\n");
    while (iter.next()) |instruction| {
        const trimmed = std.mem.trim(u8, instruction, "\r");
        var iter_instr = std.mem.splitScalar(u8, trimmed, ' ');
        const direction = iter_instr.next().?;
        const amount = iter_instr.next().?;
        const parsed_amount = try std.fmt.parseUnsigned(u32, amount, 10);
        const parsed_direction = Direction.parse(direction);
        try ret.append(.{ parsed_direction, parsed_amount });
    }
    return ret;
}

fn part1(instructions: std.ArrayList(struct { Direction, u32 }), position: *Vec2) i64 {
    for (instructions.items[0..]) |instruction| {
        instruction[0].move1(instruction[1], position);
    }
    return position.x * position.y;
}

fn part2(instructions: std.ArrayList(struct { Direction, u32 }), position: *Vec3) i64 {
    for (instructions.items[0..]) |instruction| {
        instruction[0].move2(instruction[1], position);
    }
    return position.x * position.y;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const instructions = try parse(content);
    defer instructions.deinit();
    var pos1 = Vec2{
        .x = 0,
        .y = 0,
    };
    var pos2 = Vec3{
        .x = 0,
        .y = 0,
        .z = 0,
    };
    const result1 = part1(instructions, &pos1);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = part2(instructions, &pos2);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
