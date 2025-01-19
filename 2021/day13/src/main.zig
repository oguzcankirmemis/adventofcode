const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");
const common = @import("common");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const Axis = enum { x, y };

const Instruction = struct {
    axis: Axis,
    offset: usize,
};

const Paper = struct {
    map: std.ArrayList(std.ArrayList(bool)),
    curr_instruction: usize,
    instructions: std.ArrayList(Instruction),
    curr_dots: usize,
};

fn parse(input: []const u8) !Paper {
    var paper = Paper{
        .map = std.ArrayList(std.ArrayList(bool)).init(allocator),
        .curr_instruction = 0,
        .instructions = std.ArrayList(Instruction).init(allocator),
        .curr_dots = 0,
    };
    var iterator = std.mem.splitSequence(u8, input, os_delimiter ** 2);
    const dots = iterator.next().?;
    const instructions = iterator.next().?;
    var row_max: usize = 0;
    var col_max: usize = 0;
    var indexes = std.ArrayList(common.Index2D).init(allocator);
    defer indexes.deinit();
    var dots_iterator = std.mem.splitSequence(u8, dots, os_delimiter);
    while (dots_iterator.next()) |dot| {
        var dot_iterator = std.mem.splitScalar(u8, dot, ',');
        const x = dot_iterator.next().?;
        const y = dot_iterator.next().?;
        const index = common.Index2D{
            .col = try std.fmt.parseUnsigned(usize, y, 10),
            .row = try std.fmt.parseUnsigned(usize, x, 10),
        };
        if (index.col > col_max) {
            col_max = index.col;
        }
        if (index.row > row_max) {
            row_max = index.row;
        }
        try indexes.append(index);
    }
    paper.curr_dots = indexes.items.len;
    for (0..(col_max + 1)) |_| {
        var row = std.ArrayList(bool).init(allocator);
        try row.appendNTimes(false, row_max + 1);
        try paper.map.append(row);
    }
    for (indexes.items) |index| {
        paper.map.items[index.col].items[index.row] = true;
    }
    var instructions_iterator = std.mem.splitSequence(u8, instructions, os_delimiter);
    while (instructions_iterator.next()) |instruction| {
        var instruction_iterator = std.mem.splitScalar(u8, instruction, '=');
        const axis = instruction_iterator.next().?;
        const offset = instruction_iterator.next().?;
        const instr = Instruction{
            .axis = if (axis[axis.len - 1] == 'x') Axis.x else Axis.y,
            .offset = try std.fmt.parseUnsigned(usize, offset, 10),
        };
        try paper.instructions.append(instr);
    }
    return paper;
}

fn foldX(paper: *Paper, offset: usize) !void {
    const current = paper.map;
    defer {
        for (current.items) |row| {
            row.deinit();
        }
        current.deinit();
    }
    const width = if (offset > current.items[0].items.len - 1 - offset) offset else current.items[0].items.len - 1 - offset;
    const height = current.items.len;
    var folded = std.ArrayList(std.ArrayList(bool)).init(allocator);
    for (0..height) |_| {
        var row = std.ArrayList(bool).init(allocator);
        try row.appendNTimes(false, width);
        try folded.append(row);
    }
    var curr_dots: usize = 0;
    var folded_row = width - 1;
    var mirror_row: i64 = @intCast(offset - 1);
    for ((offset + 1)..current.items[0].items.len) |row| {
        for (0..height) |col| {
            folded.items[col].items[folded_row] = current.items[col].items[row];
            if (mirror_row >= 0) {
                folded.items[col].items[folded_row] = folded.items[col].items[folded_row] or current.items[col].items[@intCast(mirror_row)];
            }
            if (folded.items[col].items[folded_row]) {
                curr_dots += 1;
            }
        }
        mirror_row -= 1;
        if (folded_row > 0) {
            folded_row -= 1;
        }
    }
    while (mirror_row >= 0) : (mirror_row -= 1) {
        for (0..height) |col| {
            folded.items[col].items[@intCast(mirror_row)] = current.items[col].items[@intCast(mirror_row)];
            if (folded.items[col].items[@intCast(mirror_row)]) {
                curr_dots += 1;
            }
        }
    }
    paper.map = folded;
    paper.curr_dots = curr_dots;
}

fn foldY(paper: *Paper, offset: usize) !void {
    const current = paper.map;
    defer {
        for (current.items) |row| {
            row.deinit();
        }
        current.deinit();
    }
    const width = current.items[0].items.len;
    const height = if (offset > current.items.len - 1 - offset) offset else current.items.len - 1 - offset;
    var folded = std.ArrayList(std.ArrayList(bool)).init(allocator);
    for (0..height) |_| {
        var row = std.ArrayList(bool).init(allocator);
        try row.appendNTimes(false, width);
        try folded.append(row);
    }
    var curr_dots: usize = 0;
    var folded_col = height - 1;
    var mirror_col: i64 = @intCast(offset - 1);
    for ((offset + 1)..current.items.len) |col| {
        for (0..width) |row| {
            folded.items[folded_col].items[row] = current.items[col].items[row];
            if (mirror_col >= 0) {
                folded.items[folded_col].items[row] = folded.items[folded_col].items[row] or current.items[@intCast(mirror_col)].items[row];
            }
            if (folded.items[folded_col].items[row]) {
                curr_dots += 1;
            }
        }
        mirror_col -= 1;
        if (folded_col > 0) {
            folded_col -= 1;
        }
    }
    while (mirror_col >= 0) : (mirror_col -= 1) {
        for (0..width) |row| {
            folded.items[@intCast(mirror_col)].items[row] = current.items[@intCast(mirror_col)].items[row];
            if (folded.items[@intCast(mirror_col)].items[row]) {
                curr_dots += 1;
            }
        }
    }
    paper.map = folded;
    paper.curr_dots = curr_dots;
}

fn fold(paper: *Paper) !void {
    const instr = paper.instructions.items[paper.curr_instruction];
    try switch (instr.axis) {
        Axis.x => foldX(paper, instr.offset),
        Axis.y => foldY(paper, instr.offset),
    };
    paper.curr_instruction += 1;
}

fn part1(paper: *Paper) !usize {
    try fold(paper);
    return paper.curr_dots;
}

fn part2(paper: *Paper) ![]const u8 {
    while (paper.curr_instruction < paper.instructions.items.len) {
        try fold(paper);
    }
    var ret = std.ArrayList(u8).init(allocator);
    for (paper.map.items) |row| {
        for (row.items) |dot| {
            if (dot) {
                try ret.append('#');
            } else {
                try ret.append('.');
            }
        }
        try ret.append('\n');
    }
    return ret.toOwnedSlice();
}

pub fn main() !void {
    defer {
        const check = gpa.deinit();
        if (check == .leak) {
            std.debug.print("Memory leak detected!\n", .{});
        }
    }
    const stdout = std.io.getStdOut();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    var paper = try parse(content);
    defer {
        for (paper.map.items) |row| {
            row.deinit();
        }
        paper.map.deinit();
        paper.instructions.deinit();
    }
    const result1 = try part1(&paper);
    try stdout.writer().print("Part 1: {d}\n", .{result1});
    const result2 = try part2(&paper);
    defer allocator.free(result2);
    try stdout.writer().print("Part 2:\n{s}", .{result2});
}
