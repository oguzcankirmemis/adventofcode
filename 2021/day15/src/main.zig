const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");
const common = @import("common");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const Node = struct {
    pos: common.Index2D,
    dist: u64,

    fn compare(context: void, a: Node, b: Node) std.math.Order {
        _ = context;
        return std.math.order(a.dist, b.dist);
    }
};

fn parse(input: []const u8) !std.ArrayList(std.ArrayList(u64)) {
    var ret = std.ArrayList(std.ArrayList(u64)).init(allocator);
    var iterator = std.mem.splitSequence(u8, input, os_delimiter);
    while (iterator.next()) |row| {
        var r = std.ArrayList(u64).init(allocator);
        for (row) |c| {
            try r.append(c - '0');
        }
        try ret.append(r);
    }
    return ret;
}

fn djikstra(map: std.ArrayList(std.ArrayList(u64)), source: common.Index2D, end: common.Index2D) !u64 {
    var dist = std.ArrayList(std.ArrayList(u64)).init(allocator);
    defer {
        for (dist.items) |row| {
            row.deinit();
        }
        dist.deinit();
    }
    for (0..map.items.len) |i| {
        var row = std.ArrayList(u64).init(allocator);
        for (0..map.items[i].items.len) |_| {
            try row.append(std.math.maxInt(u64));
        }
        try dist.append(row);
    }
    dist.items[source.col].items[source.row] = 0;
    var pq = std.PriorityQueue(Node, void, Node.compare).init(allocator, void{});
    defer pq.deinit();
    try pq.add(Node{ .pos = source, .dist = 0 });
    while (pq.removeOrNull()) |u| {
        if (u.pos.col == end.col and u.pos.row == end.row) {
            return u.dist;
        }
        var buffer: [4]common.Index2D = undefined;
        for (u.pos.basicNeighbours(map.items.len, map.items[u.pos.col].items.len, &buffer)) |v| {
            const alt = dist.items[u.pos.col].items[u.pos.row] + map.items[v.col].items[v.row];
            if (alt < dist.items[v.col].items[v.row]) {
                dist.items[v.col].items[v.row] = alt;
                try pq.add(Node{ .pos = v, .dist = alt });
            }
        }
    }
    unreachable;
}

fn part1(map: std.ArrayList(std.ArrayList(u64))) !u64 {
    const start = common.Index2D{
        .col = 0,
        .row = 0,
    };
    const end = common.Index2D{
        .col = map.items.len - 1,
        .row = map.items[map.items.len - 1].items.len - 1,
    };
    return try djikstra(map, start, end);
}

fn part2(map: std.ArrayList(std.ArrayList(u64))) !u64 {
    var expanded = std.ArrayList(std.ArrayList(u64)).init(allocator);
    defer {
        for (expanded.items) |row| {
            row.deinit();
        }
        expanded.deinit();
    }
    for (0..(5 * map.items.len)) |i| {
        var row = std.ArrayList(u64).init(allocator);
        try row.appendNTimes(0, map.items[i % map.items.len].items.len * 5);
        try expanded.append(row);
    }
    for (0..map.items.len) |i| {
        for (0..map.items[i].items.len) |j| {
            for (0..5) |k| {
                for (0..5) |l| {
                    const col = i + k * map.items.len;
                    const row = j + l * map.items[i].items.len;
                    expanded.items[col].items[row] = map.items[i].items[j] + k + l;
                    if (expanded.items[col].items[row] > 9) {
                        expanded.items[col].items[row] -= 9;
                    }
                }
            }
        }
    }
    const start = common.Index2D{
        .col = 0,
        .row = 0,
    };
    const end = common.Index2D{
        .col = expanded.items.len - 1,
        .row = expanded.items[expanded.items.len - 1].items.len - 1,
    };
    return try djikstra(expanded, start, end);
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
    const map = try parse(content);
    defer {
        for (map.items) |row| {
            row.deinit();
        }
        map.deinit();
    }
    const result1 = try part1(map);
    try writer.print("Part 1: {d}\n", .{result1});
    const result2 = try part2(map);
    try writer.print("Part 2: {d}\n", .{result2});
}
