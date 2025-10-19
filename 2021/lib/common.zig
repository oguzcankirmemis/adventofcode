const std = @import("std");

pub const Vec2 = struct {
    x: i64,
    y: i64,

    pub fn eql(self: Vec2, other: Vec2) bool {
        return self.x == other.x and
            self.y == other.y;
    }

    pub const Vec2KeyContext = struct {
        pub fn eql(_: Vec2KeyContext, a: Vec2, b: Vec2) bool {
            return a.eql(b);
        }

        pub fn hash(_: Vec2KeyContext, key: Vec2) u64 {
            const p1 = 7727;
            const p2 = 7741;
            const m = 7757;
            return @intCast(@mod((key.x * p1 + key.y * p2), m));
        }
    };
};

pub const Vec3 = struct {
    x: i64,
    y: i64,
    z: i64,

    pub fn turnAroundX(self: Vec3) Vec3 {
        return Vec3{ .x = self.x, .y = -self.z, .z = self.y };
    }

    pub fn turnAroundY(self: Vec3) Vec3 {
        return Vec3{ .x = -self.z, .y = self.y, .z = self.x };
    }

    pub fn turnAroundZ(self: Vec3) Vec3 {
        return Vec3{ .x = -self.y, .y = self.x, .z = self.z };
    }

    fn topPermutations(self: Vec3) [6]Vec3 {
        return [6]Vec3{
            self,
            self.turnAroundX(),
            self.turnAroundX().turnAroundX(),
            self.turnAroundX().turnAroundX().turnAroundX(),
            self.turnAroundY(),
            self.turnAroundY().turnAroundY().turnAroundY(),
        };
    }

    pub fn num_orientations() u64 {
        return 24;
    }

    pub fn orientations(self: Vec3) [24]Vec3 {
        var ret: [24]Vec3 = undefined;
        const tops = self.topPermutations();
        for (0..6) |i| {
            var curr = tops[i];
            for (0..4) |j| {
                ret[4 * i + j] = curr;
                curr = curr.turnAroundZ();
            }
        }
        return ret;
    }

    pub fn add(self: Vec3, other: Vec3) Vec3 {
        return Vec3{
            .x = self.x + other.x,
            .y = self.y + other.y,
            .z = self.z + other.z,
        };
    }

    pub fn substract(self: Vec3, other: Vec3) Vec3 {
        return Vec3{
            .x = self.x - other.x,
            .y = self.y - other.y,
            .z = self.z - other.z,
        };
    }

    pub fn eql(self: Vec3, other: Vec3) bool {
        return self.x == other.x and self.y == other.y and self.z == other.z;
    }

    pub fn manhattan_distance(self: Vec3, other: Vec3) u64 {
        var x_dist = self.x - other.x;
        x_dist = std.math.sign(x_dist) * x_dist;
        var y_dist = self.y - other.y;
        y_dist = std.math.sign(y_dist) * y_dist;
        var z_dist = self.z - other.z;
        z_dist = std.math.sign(z_dist) * z_dist;
        return @intCast(x_dist + y_dist + z_dist);
    }

    pub const Vec3KeyContext = struct {
        pub fn eql(_: Vec3KeyContext, a: Vec3, b: Vec3) bool {
            return a.eql(b);
        }

        pub fn hash(_: Vec3KeyContext, key: Vec3) u64 {
            const p1 = 7727;
            const p2 = 7741;
            const p3 = 7753;
            const m = 7757;
            return @intCast(@mod((key.x * p1 + key.y * p2 + key.z * p3), m));
        }
    };
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
