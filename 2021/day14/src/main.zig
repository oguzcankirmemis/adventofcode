const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const Polymer = struct {
    formula: std.ArrayList(u8),
    counts: ['Z' - 'A' + 1]['Z' - 'A' + 1]u64,
    rules: std.AutoHashMap([2]u8, u8),

    fn applyRulesSlow(self: *Polymer) !void {
        const curr = self.formula;
        defer curr.deinit();
        var next = std.ArrayList(u8).init(allocator);
        try next.append(self.formula.items[0]);
        for (0..(self.formula.items.len - 1)) |i| {
            const key: [2]u8 = [_]u8{ self.formula.items[i], self.formula.items[i + 1] };
            if (self.rules.get(key)) |c| {
                try next.append(c);
            }
            try next.append(self.formula.items[i + 1]);
        }
        self.formula = next;
    }

    fn appyRulesFast(self: *Polymer) void {
        var new = [1]['Z' - 'A' + 1]u64{[_]u64{0} ** ('Z' - 'A' + 1)} ** ('Z' - 'A' + 1);
        for (0..('Z' - 'A' + 1)) |i| {
            const c1: u8 = @intCast('A' + i);
            for (0..('Z' - 'A' + 1)) |j| {
                const c2: u8 = @intCast('A' + j);
                if (self.counts[i][j] > 0) {
                    if (self.rules.get([2]u8{ c1, c2 })) |c| {
                        new[i][c - 'A'] += self.counts[i][j];
                        new[c - 'A'][j] += self.counts[i][j];
                    } else {
                        new[i][j] += self.counts[i][j];
                    }
                }
            }
        }
        self.counts = new;
    }
};

fn parse(input: []const u8) !Polymer {
    var ret = Polymer{
        .formula = std.ArrayList(u8).init(allocator),
        .counts = [1]['Z' - 'A' + 1]u64{[_]u64{0} ** ('Z' - 'A' + 1)} ** ('Z' - 'A' + 1),
        .rules = std.AutoHashMap([2]u8, u8).init(allocator),
    };
    var iterator = std.mem.splitSequence(u8, input, os_delimiter ** 2);
    const formula = iterator.next().?;
    try ret.formula.appendSlice(formula);
    for (0..(formula.len - 1)) |i| {
        const c1 = formula[i] - 'A';
        const c2 = formula[i + 1] - 'A';
        ret.counts[c1][c2] += 1;
    }
    const rules = iterator.next().?;
    var rules_iterator = std.mem.splitSequence(u8, rules, os_delimiter);
    while (rules_iterator.next()) |rule| {
        try ret.rules.put(rule[0..2].*, rule[rule.len - 1]);
    }
    return ret;
}

fn part1(polymer: *Polymer) !u64 {
    for (0..10) |_| {
        try polymer.applyRulesSlow();
    }
    var counts: ['Z' - 'A' + 1]u64 = [_]u64{0} ** ('Z' - 'A' + 1);
    for (polymer.formula.items) |c| {
        counts[c - 'A'] += 1;
    }
    var max: u64 = 0;
    var min: u64 = std.math.maxInt(u64);
    for (0..('Z' - 'A' + 1)) |i| {
        if (counts[i] > max) {
            max = counts[i];
        }
        if (counts[i] != 0 and counts[i] < min) {
            min = counts[i];
        }
    }
    return max - min;
}

fn part2(polymer: *Polymer) u64 {
    for (0..40) |_| {
        polymer.appyRulesFast();
    }
    var counts: ['Z' - 'A' + 1]u64 = [_]u64{0} ** ('Z' - 'A' + 1);
    counts[polymer.formula.items[0] - 'A'] += 1;
    for (0..('Z' - 'A' + 1)) |i| {
        for (0..('Z' - 'A' + 1)) |j| {
            counts[j] += polymer.counts[i][j];
        }
    }
    var max: u64 = 0;
    var min: u64 = std.math.maxInt(u64);
    for (0..('Z' - 'A' + 1)) |i| {
        if (counts[i] > max) {
            max = counts[i];
        }
        if (counts[i] != 0 and counts[i] < min) {
            min = counts[i];
        }
    }
    return max - min;
}

pub fn main() !void {
    defer {
        const check = gpa.deinit();
        if (check == .leak) {
            std.debug.print("Memory leak detected!\n", .{});
        }
    }
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    var polymer = try parse(content);
    defer {
        polymer.formula.deinit();
        polymer.rules.deinit();
    }
    const result1 = try part1(&polymer);
    try writer.print("Part 1: {d}\n", .{result1});
    const result2 = part2(&polymer);
    try writer.print("Part 2: {d}\n", .{result2});
}
