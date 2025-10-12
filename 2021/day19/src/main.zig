const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");
const common = @import("common");
const Vec3 = common.Vec3;

const MAX_FILE_SIZE = 1024 * 1024;
const OVERLAP_THRESHOLD = 12;
const SCANNER_RANGE = 1000;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const Result = struct {
    num_of_beacons: u64,
    max_manhattan_distance: u64,
};

const Scanner = struct {
    id: u64,
    beacons: std.ArrayList(Vec3),
    orientation: ?usize,

    fn deinit(self: Scanner) void {
        defer self.beacons.deinit();
    }

    fn parse(scanner_str: []const u8) !Scanner {
        var iter = std.mem.splitSequence(u8, scanner_str, os_delimiter);
        const id_str = iter.next().?;
        var id_iter = std.mem.splitScalar(u8, id_str, ' ');
        _ = id_iter.next();
        _ = id_iter.next();
        const id = try std.fmt.parseUnsigned(u64, id_iter.next().?, 10);
        var scanner: Scanner = .{
            .id = id,
            .beacons = std.ArrayList(Vec3).init(allocator),
            .orientation = null,
        };
        while (iter.next()) |beacon| {
            var beacon_iter = std.mem.splitScalar(u8, beacon, ',');
            const x = try std.fmt.parseInt(i64, beacon_iter.next().?, 10);
            const y = try std.fmt.parseInt(i64, beacon_iter.next().?, 10);
            const z = try std.fmt.parseInt(i64, beacon_iter.next().?, 10);
            try scanner.beacons.append(.{ .x = x, .y = y, .z = z });
        }
        return scanner;
    }
};

const UnionFind = struct {
    parents: []u64,
    ranks: []u64,
    transformations: []Vec3,
};

fn parse(input: []const u8) !std.ArrayList(Scanner) {
    var iter = std.mem.splitSequence(u8, input, os_delimiter ** 2);
    var ret = std.ArrayList(Scanner).init(allocator);
    while (iter.next()) |scanner| {
        try ret.append(try Scanner.parse(scanner));
    }
    return ret;
}

fn find(uf: UnionFind, x: u64) u64 {
    if (uf.parents[x] != x) {
        const prev = uf.parents[x];
        uf.parents[x] = find(uf, prev);
        uf.transformations[x] = uf.transformations[prev].add(uf.transformations[x]);
        return uf.parents[x];
    }
    return x;
}

fn merge(uf: UnionFind, beacon_from_x: Vec3, beacon_from_y: Vec3, x: u64, y: u64) void {
    const root_x = find(uf, x);
    const root_y = find(uf, y);
    if (root_x == root_y) {
        return;
    }
    if (uf.ranks[root_x] < uf.ranks[root_y]) {
        merge(uf, beacon_from_y, beacon_from_x, y, x);
        return;
    }
    uf.parents[root_y] = root_x;
    const beacon_from_root_x = beacon_from_x.add(uf.transformations[x]);
    const beacon_from_root_y = beacon_from_y.add(uf.transformations[y]);
    uf.transformations[root_y] = beacon_from_root_x.substract(beacon_from_root_y);
    if (uf.ranks[root_x] == uf.ranks[root_y]) {
        uf.ranks[root_x] += 1;
    }
}

fn is_beacon_in_range(beacon: Vec3) bool {
    return -SCANNER_RANGE <= beacon.x and beacon.x <= SCANNER_RANGE and
        -SCANNER_RANGE <= beacon.y and beacon.y <= SCANNER_RANGE and
        -SCANNER_RANGE <= beacon.z and beacon.z <= SCANNER_RANGE;
}

fn exist_beacon(scanner: *const Scanner, beacon: Vec3, orientation: usize) bool {
    for (scanner.beacons.items) |other| {
        if (beacon.eql(other.orientations()[orientation])) {
            return true;
        }
    }
    return false;
}

fn overlapping_beacons(s1: *const Scanner, s2: *const Scanner, o1: usize, o2: usize, fixed_beacon_from_s1: usize, s1_to_s2: Vec3) bool {
    var overlap_count: usize = 1;
    for ((fixed_beacon_from_s1 + 1)..s1.beacons.items.len) |b1| {
        if (overlap_count >= OVERLAP_THRESHOLD) {
            return true;
        }
        if (s1.beacons.items.len - b1 + overlap_count < OVERLAP_THRESHOLD) {
            return false;
        }
        const beacon1 = s1.beacons.items[b1].orientations()[o1];
        const beacon1_from_s2 = beacon1.substract(s1_to_s2);
        if (is_beacon_in_range(beacon1_from_s2)) {
            if (!exist_beacon(s2, beacon1_from_s2, o2)) {
                return false;
            }
            overlap_count += 1;
        }
    }
    return overlap_count >= OVERLAP_THRESHOLD;
}

fn overlap_with_selected_beacons(uf: UnionFind, s1: *const Scanner, s2: *const Scanner, o1: usize, o2: usize) bool {
    for (0..s1.beacons.items.len) |b1| {
        for (0..s2.beacons.items.len) |b2| {
            const beacon1 = s1.beacons.items[b1].orientations()[o1];
            const beacon2 = s2.beacons.items[b2].orientations()[o2];
            const s1_to_s2 = beacon1.substract(beacon2);
            if (overlapping_beacons(s1, s2, o1, o2, b1, s1_to_s2)) {
                merge(uf, beacon1, beacon2, s1.id, s2.id);
                return true;
            }
        }
    }
    return false;
}

fn find_orientation(uf: UnionFind, s1: *Scanner, s2: *Scanner, start_orientation: u64) !bool {
    const o1 = s1.orientation.?;
    for (start_orientation..Vec3.num_orientations()) |o2| {
        if (overlap_with_selected_beacons(uf, s1, s2, o1, o2)) {
            s2.orientation = o2;
            return true;
        }
    }
    return false;
}

fn solve_part2(uf: UnionFind) u64 {
    var max_manhattan_distance: u64 = 0;
    for (0..uf.transformations.len) |i| {
        for ((i + 1)..uf.transformations.len) |j| {
            const curr_manhattan_distance = uf.transformations[i].manhattan_distance(uf.transformations[j]);
            if (curr_manhattan_distance > max_manhattan_distance) {
                max_manhattan_distance = curr_manhattan_distance;
            }
        }
    }
    return max_manhattan_distance;
}

fn solve_both_parts(input: std.ArrayList(Scanner)) !Result {
    if (input.items.len == 0) {
        return Result{
            .num_of_beacons = 0,
            .max_manhattan_distance = 0,
        };
    }
    var parents = try std.ArrayList(u64).initCapacity(allocator, input.items.len);
    defer parents.deinit();
    var ranks = try std.ArrayList(u64).initCapacity(allocator, input.items.len);
    defer ranks.deinit();
    var transformations = try std.ArrayList(Vec3).initCapacity(allocator, input.items.len);
    defer transformations.deinit();
    for (0..input.items.len) |i| {
        try parents.append(i);
        try ranks.append(0);
        try transformations.append(Vec3{ .x = 0, .y = 0, .z = 0 });
    }
    const uf = UnionFind{
        .parents = parents.items,
        .ranks = ranks.items,
        .transformations = transformations.items,
    };
    var stack = std.ArrayList(*Scanner).init(allocator);
    defer stack.deinit();
    try stack.append(&input.items[0]);
    stack.items[0].orientation = 0;
    var working_set = std.ArrayList(*Scanner).init(allocator);
    defer working_set.deinit();
    for (1..input.items.len) |i| {
        try working_set.append(&input.items[i]);
    }
    while (stack.items.len != input.items.len) {
        var j: u64 = 0;
        var new_scanner_found = false;
        while (j < working_set.items.len) {
            const s2 = working_set.items[j];
            for (0..stack.items.len) |i| {
                const s1 = stack.items[i];
                if (try find_orientation(uf, s1, s2, 0)) {
                    _ = working_set.swapRemove(j);
                    try stack.append(s2);
                    new_scanner_found = true;
                    for (working_set.items) |s| {
                        s.orientation = null;
                    }
                    break;
                }
            }
            if (new_scanner_found) {
                break;
            }
            j += 1;
        }
        if (!new_scanner_found) {
            const s2 = stack.pop();
            uf.parents[s2.id] = s2.id;
            uf.ranks[s2.id] = 0;
            uf.transformations[s2.id] = Vec3{ .x = 0, .y = 0, .z = 0 };
            for (0..stack.items.len) |i| {
                const s1 = stack.items[i];
                if (try find_orientation(uf, s1, s2, s2.orientation.? + 1)) {
                    try stack.append(s2);
                } else {
                    try working_set.append(s2);
                }
            }
        }
    }
    const HashsetType = std.HashMap(Vec3, bool, Vec3.Vec3KeyContext, std.hash_map.default_max_load_percentage);
    var set = HashsetType.init(allocator);
    defer set.deinit();
    for (input.items) |scanner| {
        for (scanner.beacons.items) |beacon| {
            const oriented_beacon = beacon.orientations()[scanner.orientation.?];
            const oriented_beacon_from_root = oriented_beacon.add(uf.transformations[scanner.id]);
            try set.put(oriented_beacon_from_root, true);
        }
    }
    const result = Result{
        .num_of_beacons = set.count(),
        .max_manhattan_distance = solve_part2(uf),
    };
    return result;
}

pub fn main() !void {
    const writer = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const scanners = try parse(content);
    defer {
        for (scanners.items) |scanner| {
            scanner.deinit();
        }
        scanners.deinit();
    }
    const result = try solve_both_parts(scanners);
    try writer.print("Part 1: {d}\n", .{result.num_of_beacons});
    try writer.print("Part 2: {d}\n", .{result.max_manhattan_distance});
}
