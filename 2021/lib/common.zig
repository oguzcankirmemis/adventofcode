const std = @import("std");

pub const Vec2 = struct {
    x: i64,
    y: i64,
};

pub const Index2D = struct {
    col: usize,
    row: usize,

    pub fn basicNeighbours(self: Index2D, height: usize, width: usize, buffer: *[4]Index2D) []const Index2D {
        var i: usize = 0;
        if (self.col > 0) {
            buffer[i] = .{
                .col = self.col - 1,
                .row = self.row,
            };
            i += 1;
        }
        if (self.row > 0) {
            buffer[i] = .{
                .col = self.col,
                .row = self.row - 1,
            };
            i += 1;
        }
        if (self.row + 1 < width) {
            buffer[i] = .{
                .col = self.col,
                .row = self.row + 1,
            };
            i += 1;
        }
        if (self.col + 1 < height) {
            buffer[i] = .{
                .col = self.col + 1,
                .row = self.row,
            };
            i += 1;
        }
        return buffer[0..i];
    }

    pub fn neighbours(self: Index2D, height: usize, width: usize, buffer: *[8]Index2D) []const Index2D {
        var i: usize = 0;
        if (self.col > 0 and self.row > 0) {
            buffer[i] = .{
                .col = self.col - 1,
                .row = self.row - 1,
            };
            i += 1;
        }
        if (self.col > 0) {
            buffer[i] = .{
                .col = self.col - 1,
                .row = self.row,
            };
            i += 1;
        }
        if (self.col > 0 and self.row + 1 < width) {
            buffer[i] = .{
                .col = self.col - 1,
                .row = self.row + 1,
            };
            i += 1;
        }
        if (self.row > 0) {
            buffer[i] = .{
                .col = self.col,
                .row = self.row - 1,
            };
            i += 1;
        }
        if (self.row + 1 < width) {
            buffer[i] = .{
                .col = self.col,
                .row = self.row + 1,
            };
            i += 1;
        }
        if (self.col + 1 < height and self.row > 0) {
            buffer[i] = .{
                .col = self.col + 1,
                .row = self.row - 1,
            };
            i += 1;
        }
        if (self.col + 1 < height) {
            buffer[i] = .{
                .col = self.col + 1,
                .row = self.row,
            };
            i += 1;
        }
        if (self.col + 1 < height and self.row + 1 < width) {
            buffer[i] = .{
                .col = self.col + 1,
                .row = self.row + 1,
            };
            i += 1;
        }
        return buffer[0..i];
    }
};

pub const Graph = struct {
    adj: std.StringHashMap(std.ArrayList([]const u8)),
    allocator: std.mem.Allocator,
    empty: [0][]const u8,

    pub fn init(allocator: std.mem.Allocator) Graph {
        const self = Graph{
            .adj = std.StringHashMap(std.ArrayList([]const u8)).init(allocator),
            .allocator = allocator,
            .empty = [0][]const u8{},
        };
        return self;
    }

    pub fn neighbours(self: Graph, node: []const u8) [][]const u8 {
        if (self.adj.get(node)) |list| {
            return list.items;
        }
        return &self.empty;
    }

    pub fn addEdge(self: *Graph, from: []const u8, to: []const u8) !void {
        var list = try self.adj.getOrPut(from);
        if (!list.found_existing) {
            list.value_ptr.* = std.ArrayList([]const u8).init(self.allocator);
        }
        try list.value_ptr.append(to);
    }

    pub fn deinit(self: *Graph) void {
        var iterator = self.adj.valueIterator();
        while (iterator.next()) |list| {
            list.deinit();
        }
        self.adj.deinit();
    }
};
