const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");
const common = @import("common");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.target.os.tag == .windows) "\r\n" else "\n";

fn parse(input: []const u8) !common.Graph {
    var graph = common.Graph.init(allocator);
    var iterator = std.mem.splitSequence(u8, input, os_delimiter);
    while (iterator.next()) |line| {
        var line_iterator = std.mem.splitScalar(u8, line, '-');
        const u = line_iterator.next().?;
        const v = line_iterator.next().?;
        try graph.addEdge(u, v);
        try graph.addEdge(v, u);
    }
    return graph;
}

fn isBigCave(node: []const u8) bool {
    return node[0] < 'a';
}

fn copyVisited(visited: std.StringHashMap(void)) !std.StringHashMap(void) {
    const ret = std.StringHashMap(void).init(allocator);
    var iterator = visited.keyIterator();
    for (iterator.next()) |key| {
        try ret.put(key, void{});
    }
    return ret;
}

fn hash(graph: common.Graph, curr: []const u8, visited: std.StringHashMap(void), small_cave_visited_twice_before: bool) !std.ArrayList(u8) {
    var ret = std.ArrayList(u8).init(allocator);
    try ret.appendSlice(curr);
    try ret.append('-');
    var iterator = graph.adj.keyIterator();
    while (iterator.next()) |key| {
        if (isBigCave(key.*)) {
            try ret.append('b');
        } else if (visited.get(key.*) == null) {
            try ret.append('f');
        } else {
            try ret.append('t');
        }
    }
    if (small_cave_visited_twice_before) {
        try ret.append('t');
    } else {
        try ret.append('f');
    }
    return ret;
}

fn dfs(graph: common.Graph, start: []const u8, end: []const u8, visited: *std.StringHashMap(void), small_cave_visited_twice_before: bool, dp: *std.StringHashMap(u64)) !u64 {
    if (std.mem.eql(u8, start, end)) {
        return 1;
    }
    try visited.put(start, void{});
    const dp_hash = try blk: {
        var hash_list = try hash(graph, start, visited.*, small_cave_visited_twice_before);
        break :blk hash_list.toOwnedSlice();
    };
    const lookup = dp.get(dp_hash);
    if (lookup) |result| {
        defer allocator.free(dp_hash);
        _ = visited.remove(start);
        return result;
    }
    var ret: u64 = 0;
    for (graph.neighbours(start)) |neighbour| {
        const is_visited = visited.get(neighbour) != null;
        const is_big = isBigCave(neighbour);
        if (is_big or !is_visited) {
            ret += try dfs(graph, neighbour, end, visited, small_cave_visited_twice_before, dp);
        } else if (!is_big and is_visited and !small_cave_visited_twice_before and !std.mem.eql(u8, neighbour, "start") and !std.mem.eql(u8, neighbour, "end")) {
            ret += try dfs(graph, neighbour, end, visited, true, dp);
            try visited.put(neighbour, void{});
        }
    }
    try dp.put(dp_hash, ret);
    _ = visited.remove(start);
    return ret;
}

fn part1(graph: common.Graph) !u64 {
    var visited = std.StringHashMap(void).init(allocator);
    defer visited.deinit();
    var dp = std.StringHashMap(u64).init(allocator);
    defer {
        var iterator = dp.keyIterator();
        while (iterator.next()) |key| {
            allocator.free(key.*);
        }
        dp.deinit();
    }
    return dfs(graph, "start", "end", &visited, true, &dp);
}

fn part2(graph: common.Graph) !u64 {
    var visited = std.StringHashMap(void).init(allocator);
    defer visited.deinit();
    var dp = std.StringHashMap(u64).init(allocator);
    defer {
        var iterator = dp.keyIterator();
        while (iterator.next()) |key| {
            allocator.free(key.*);
        }
        dp.deinit();
    }
    return dfs(graph, "start", "end", &visited, false, &dp);
}

pub fn main() !void {
    defer {
        const check = gpa.deinit();
        if (check == .leak) {
            std.debug.print("Leak detected!\n", .{});
        }
    }
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    var graph = try parse(content);
    defer graph.deinit();
    const result1 = try part1(graph);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = try part2(graph);
    try stdout.writer().print("Part 2: {d}\n", .{result2});
}
