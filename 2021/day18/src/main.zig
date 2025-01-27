const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "n";

const Leaf = struct {
    number: u64,
    next: ?*Leaf,
    prev: ?*Leaf,
    root: *SnailfishNumber,
};

const SnailfishNumber = struct {
    left_snailfish_number: ?*SnailfishNumber,
    right_snailfish_number: ?*SnailfishNumber,
    left_leaf: ?*Leaf,
    right_leaf: ?*Leaf,
    root: ?*SnailfishNumber,

    fn deinit(self: *SnailfishNumber) void {
        if (self.left_leaf) |leaf| {
            allocator.destroy(leaf);
        } else {
            self.left_snailfish_number.?.deinit();
        }
        if (self.right_leaf) |leaf| {
            allocator.destroy(leaf);
        } else {
            self.right_snailfish_number.?.deinit();
        }
        allocator.destroy(self);
    }

    fn add(self: *SnailfishNumber, operand: *SnailfishNumber) !*SnailfishNumber {
        var root = try allocator.create(SnailfishNumber);
        root.left_leaf = null;
        root.right_leaf = null;
        root.left_snailfish_number = self;
        root.right_snailfish_number = operand;
        root.root = null;
        self.root = root;
        operand.root = root;
        var left_curr = self;
        while (left_curr.right_leaf == null) {
            left_curr = left_curr.right_snailfish_number.?;
        }
        const left_leaf = left_curr.right_leaf.?;
        var right_curr = operand;
        while (right_curr.left_leaf == null) {
            right_curr = right_curr.left_snailfish_number.?;
        }
        const right_leaf = right_curr.left_leaf.?;
        left_leaf.next = right_leaf;
        right_leaf.prev = left_leaf;
        try reduce(self);
        return root;
    }

    fn magnitude(self: *SnailfishNumber) u64 {
        var ret: u64 = 0;
        if (self.left_leaf) |leaf| {
            ret += 3 * leaf.number;
        } else {
            ret += 3 * self.left_snailfish_number.?.magnitude();
        }
        if (self.right_leaf) |leaf| {
            ret += 2 * leaf.number;
        } else {
            ret += 2 * self.right_snailfish_number.?.magnitude();
        }
        return ret;
    }
};

fn parseLeaf(input: []const u8, index: *usize, prev_leaf: *?*Leaf) !*Leaf {
    var ret = try allocator.create(Leaf);
    ret.number = 0;
    ret.prev = prev_leaf.*;
    ret.next = null;
    if (prev_leaf.*) |prev| {
        prev.next = ret;
    }
    prev_leaf.* = ret;
    while ('0' <= input[index.*] and input[index.*] <= '9') {
        ret.number = ret.number * 10 + input[index.*] - '0';
        index.* += 1;
    }
    return ret;
}

fn parseSnailfishNumber(input: []const u8, index: *usize, prev_leaf: *?*Leaf) !*SnailfishNumber {
    var ret = try allocator.create(SnailfishNumber);
    ret.left_leaf = null;
    ret.right_leaf = null;
    ret.left_snailfish_number = null;
    ret.right_snailfish_number = null;
    ret.root = null;
    index.* += 1;
    if (input[index.*] == '[') {
        ret.left_leaf = null;
        ret.left_snailfish_number = try parseSnailfishNumber(input, index, prev_leaf);
        ret.left_snailfish_number.?.root = ret;
    } else {
        ret.left_leaf = try parseLeaf(input, index, prev_leaf);
        ret.left_leaf.?.root = ret;
    }
    index.* += 1;
    if (input[index.*] == '[') {
        ret.right_snailfish_number = try parseSnailfishNumber(input, index, prev_leaf);
        ret.right_snailfish_number.?.root = ret;
    } else {
        ret.right_leaf = try parseLeaf(input, index, prev_leaf);
        ret.right_leaf.?.root = ret;
    }
    index.* += 1;
    return ret;
}

fn parse(input: []const u8) !std.ArrayList(*SnailfishNumber) {
    var ret = std.ArrayList(*SnailfishNumber).init(allocator);
    var iterator = std.mem.splitSequence(u8, input, os_delimiter);
    while (iterator.next()) |line| {
        var index: usize = 0;
        var prev_leaf: ?*Leaf = null;
        try ret.append(try parseSnailfishNumber(line, &index, &prev_leaf));
    }
    return ret;
}

fn level(leaf: *const Leaf) u64 {
    var lvl: u64 = 0;
    var current: *SnailfishNumber = leaf.root;
    while (current.root) |root| {
        current = root;
        lvl += 1;
    }
    return lvl;
}

fn explode(leftmost_leaf: *Leaf) !bool {
    var left = leftmost_leaf;
    while (left.next) |right| {
        if (left.root == right.root and level(left) == 4) {
            var newLeaf = try allocator.create(Leaf);
            newLeaf.number = 0;
            newLeaf.prev = null;
            newLeaf.next = null;
            if (left.prev) |prev| {
                prev.number += left.number;
                newLeaf.prev = prev;
                prev.next = newLeaf;
            }
            if (right.next) |next| {
                next.number += right.number;
                newLeaf.next = next;
                next.prev = newLeaf;
            }
            var child = left.root;
            defer child.deinit();
            var parent = child.root.?;
            newLeaf.root = parent;
            if (parent.left_snailfish_number == child) {
                parent.left_leaf = newLeaf;
                parent.left_snailfish_number = null;
            } else {
                parent.right_leaf = newLeaf;
                parent.right_snailfish_number = null;
            }
            return true;
        }
        left = right;
    }
    return false;
}

fn split(leftmost_leaf: *Leaf) !bool {
    var current: ?*Leaf = leftmost_leaf;
    while (current) |leaf| {
        if (leaf.number >= 10) {
            defer allocator.destroy(leaf);
            const left_number = try std.math.divFloor(u64, leaf.number, 2);
            const right_number = try std.math.divCeil(u64, leaf.number, 2);
            var left_leaf = try allocator.create(Leaf);
            var right_leaf = try allocator.create(Leaf);
            left_leaf.number = left_number;
            right_leaf.number = right_number;
            left_leaf.prev = leaf.prev;
            right_leaf.prev = left_leaf;
            left_leaf.next = right_leaf;
            right_leaf.next = leaf.next;
            if (leaf.prev) |prev| {
                prev.next = left_leaf;
            }
            if (leaf.next) |next| {
                next.prev = right_leaf;
            }
            var newChild = try allocator.create(SnailfishNumber);
            newChild.left_snailfish_number = null;
            newChild.right_snailfish_number = null;
            newChild.left_leaf = left_leaf;
            newChild.right_leaf = right_leaf;
            newChild.root = leaf.root;
            left_leaf.root = newChild;
            right_leaf.root = newChild;
            if (leaf.root.left_leaf == leaf) {
                leaf.root.left_snailfish_number = newChild;
                leaf.root.left_leaf = null;
            } else {
                leaf.root.right_snailfish_number = newChild;
                leaf.root.right_leaf = null;
            }
            return true;
        }
        current = leaf.next;
    }
    return false;
}

fn reduce(root: *SnailfishNumber) !void {
    var is_reduced = true;
    while (is_reduced) {
        var current = root;
        while (current.left_leaf == null) {
            current = current.left_snailfish_number.?;
        }
        const leftmost_leaf = current.left_leaf.?;
        var reduced = try explode(leftmost_leaf);
        if (!reduced) {
            reduced = try split(leftmost_leaf);
        }
        is_reduced = reduced;
    }
}

fn part1(numbers: std.ArrayList(*SnailfishNumber)) !u64 {
    var root = numbers.items[0];
    defer root.deinit();
    for (1..numbers.items.len) |i| {
        root = try root.add(numbers.items[i]);
    }
    return root.magnitude();
}

fn part2(input: []const u8) !u64 {
    var ret: u64 = 0;
    var left_iterator = std.mem.splitSequence(u8, input, os_delimiter);
    while (left_iterator.next()) |left_line| {
        var right_iterator = std.mem.splitSequence(u8, input, os_delimiter);
        while (right_iterator.next()) |right_line| {
            if (left_iterator.index == right_iterator.index) {
                continue;
            }
            var index: usize = 0;
            var prev_leaf: ?*Leaf = null;
            const left = try parseSnailfishNumber(left_line, &index, &prev_leaf);
            index = 0;
            prev_leaf = null;
            const right = try parseSnailfishNumber(right_line, &index, &prev_leaf);
            const sum = try left.add(right);
            defer sum.deinit();
            const magnitude = sum.magnitude();
            if (magnitude > ret) {
                ret = magnitude;
            }
        }
    }
    return ret;
}

pub fn main() !void {
    defer {
        if (gpa.deinit() == .leak) {
            std.debug.print("Memory leak detected!\n", .{});
        }
    }
    const writer = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const snailfish_numbers1 = try parse(content);
    defer snailfish_numbers1.deinit();
    const result1 = try part1(snailfish_numbers1);
    try writer.print("Part 1: {d}\n", .{result1});
    const result2 = try part2(content);
    try writer.print("Part 2: {d}\n", .{result2});
}
