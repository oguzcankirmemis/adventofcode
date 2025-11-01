const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const EAST = '>';
const SOUTH = 'v';

const SeaCucumbers = struct {
    east: std.AutoArrayHashMap([2]usize, bool),
    south: std.AutoArrayHashMap([2]usize, bool),
    rows: usize,
    cols: usize,

    fn debug(self: *const SeaCucumbers) void {
        for (0..self.rows) |i| {
            for (0..self.cols) |j| {
                if (self.east.contains([_]usize{ i, j })) {
                    std.debug.print(">", .{});
                } else if (self.south.contains([_]usize{ i, j })) {
                    std.debug.print("v", .{});
                } else {
                    std.debug.print(".", .{});
                }
            }
            std.debug.print("\n", .{});
        }
    }
};

fn simulate_east(map: *SeaCucumbers, copy: *SeaCucumbers) !usize {
    var ret: usize = 0;
    var iter = map.east.iterator();
    while (iter.next()) |cucumber| {
        const row = cucumber.key_ptr.*[0];
        const col = cucumber.key_ptr.*[1];
        const n_col = (col + 1) % map.cols;
        const key = [_]usize{ row, n_col };
        if (!map.east.contains(key) and !map.south.contains(key)) {
            try copy.east.put(key, true);
            ret += 1;
        } else {
            try copy.east.put(cucumber.key_ptr.*, true);
        }
    }
    return ret;
}

fn simulate_south(map: *SeaCucumbers, copy: *SeaCucumbers) !usize {
    var ret: usize = 0;
    var iter = map.south.iterator();
    while (iter.next()) |cucumber| {
        const row = cucumber.key_ptr.*[0];
        const col = cucumber.key_ptr.*[1];
        const n_row = (row + 1) % map.rows;
        const key = [_]usize{ n_row, col };
        if (!copy.east.contains(key) and !map.south.contains(key)) {
            try copy.south.put(key, true);
            ret += 1;
        } else {
            try copy.south.put(cucumber.key_ptr.*, true);
        }
    }
    return ret;
}

fn stabilize(map: *SeaCucumbers) !u64 {
    var copy = SeaCucumbers{
        .east = std.AutoArrayHashMap([2]usize, bool).init(allocator),
        .south = std.AutoArrayHashMap([2]usize, bool).init(allocator),
        .rows = map.rows,
        .cols = map.cols,
    };
    var ptr1 = map;
    var ptr2 = &copy;
    var steps: u64 = 1;
    while (try simulate_east(ptr1, ptr2) + try simulate_south(ptr1, ptr2) > 0) {
        ptr1.east.clearRetainingCapacity();
        ptr1.south.clearRetainingCapacity();
        const tmp = ptr1;
        ptr1 = ptr2;
        ptr2 = tmp;
        steps += 1;
    }
    return steps;
}

fn parse(input: []const u8) !SeaCucumbers {
    var ret = SeaCucumbers{
        .east = std.AutoArrayHashMap([2]usize, bool).init(allocator),
        .south = std.AutoArrayHashMap([2]usize, bool).init(allocator),
        .cols = 0,
        .rows = 0,
    };
    var iter = std.mem.splitSequence(u8, input, os_delimiter);
    var i: usize = 0;
    while (iter.next()) |row| {
        ret.cols = row.len;
        for (row, 0..) |c, j| {
            if (c == EAST) {
                try ret.east.put([_]usize{ i, j }, true);
            }
            if (c == SOUTH) {
                try ret.south.put([_]usize{ i, j }, true);
            }
        }
        i += 1;
    }
    ret.rows = i;
    return ret;
}

pub fn main() !void {
    const writer = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    var map = try parse(content);
    defer {
        map.east.deinit();
        map.south.deinit();
    }
    const result1 = try stabilize(&map);
    try writer.print("Part 1: {d}\n", .{result1});
}
