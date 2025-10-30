const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const EMPTY: u8 = '.';

const Board = struct {
    hallway: [11]u8,
    stacks: [4]std.ArrayList(u8),
    max_stack_len: usize,
    energy_cost: u64,
    explored: std.AutoHashMap([2]u64, u64),

    fn is_home_column(col: usize) bool {
        return col == 2 or col == 4 or col == 6 or col == 8;
    }

    fn stack_to_column(index: usize) usize {
        return 2 + index * 2;
    }

    fn stack_to_target_species(index: usize) u8 {
        return @intCast(index + 'A');
    }

    fn species_to_target_stack(species: u8) usize {
        return species - 'A';
    }

    fn species_to_home_column(species: u8) usize {
        return stack_to_column(species - 'A');
    }

    fn stack_occupiable(self: *const Board, index: usize) bool {
        const species = stack_to_target_species(index);
        for (self.stacks[index].items) |c| {
            if (c != species) {
                return false;
            }
        }
        return true;
    }

    fn is_hallway_free(self: *const Board, from: usize, to: usize) bool {
        if (from < to) {
            var index = from + 1;
            while (index <= to) : (index += 1) {
                if (self.hallway[index] != EMPTY) {
                    return false;
                }
            }
        } else {
            var index = from - 1;
            while (index >= to) : (index -= 1) {
                if (self.hallway[index] != EMPTY) {
                    return false;
                }
            }
        }
        return true;
    }

    fn can_go_home(self: *const Board, species: u8, col: usize) bool {
        const home_stack = species_to_target_stack(species);
        const home_column = species_to_home_column(species);
        if (!self.stack_occupiable(home_stack)) {
            return false;
        }
        return is_hallway_free(self, col, home_column);
    }

    fn species_cost(species: u8) u64 {
        if (species == 'A') {
            return 1;
        }
        if (species == 'B') {
            return 10;
        }
        if (species == 'C') {
            return 100;
        }
        if (species == 'D') {
            return 1000;
        }
        unreachable;
    }

    fn stack_cost(self: *const Board, stack: *const std.ArrayList(u8), species: u8) u64 {
        return species_cost(species) * (self.max_stack_len - stack.items.len + 1);
    }

    fn hallway_cost(species: u8, from: usize, to: usize) u64 {
        if (from > to) {
            return hallway_cost(species, to, from);
        }
        return species_cost(species) * (to - from);
    }

    fn is_solved(self: *const Board) bool {
        const species = [_]u8{ 'A', 'B', 'C', 'D' };
        for (self.stacks, 0..) |stack, i| {
            if (stack.items.len != self.max_stack_len) {
                return false;
            }
            for (stack.items) |a| {
                if (a != species[i]) {
                    return false;
                }
            }
        }
        return true;
    }

    fn encode_hallway(self: *const Board) u64 {
        var ret: u64 = 0;
        var base: u64 = 1;
        for (self.hallway) |s| {
            const val: u64 = if (s == EMPTY) 4 else s - 'A';
            ret += val * base;
            base *= 5;
        }
        return ret;
    }

    fn encode_stacks(self: *const Board) u64 {
        var ret: u64 = 0;
        var base: u64 = 1;
        for (self.stacks) |stack| {
            for (0..self.max_stack_len) |i| {
                if (i >= stack.items.len) {
                    ret += 4 * base;
                } else {
                    const val = stack.items[i] - 'A';
                    ret += val * base;
                }
                base *= 5;
            }
        }
        return ret;
    }

    fn encode(self: *const Board) [2]u64 {
        return [_]u64{ self.encode_stacks(), self.encode_hallway() };
    }

    fn compute_minimal_energy(self: *Board) !u64 {
        const hash = self.encode();
        const prev_result = try self.explored.getOrPut(hash);
        if (prev_result.found_existing and prev_result.value_ptr.* <= self.energy_cost) {
            return std.math.maxInt(u64);
        } else {
            prev_result.value_ptr.* = self.energy_cost;
        }

        if (self.is_solved()) {
            return self.energy_cost;
        }

        for (self.hallway, 0..) |s, i| {
            if (s != EMPTY and self.can_go_home(s, i)) {
                const s_index = species_to_target_stack(s);
                const h_cost = hallway_cost(s, i, stack_to_column(s_index));
                try self.stacks[s_index].append(s);
                const s_cost = self.stack_cost(&self.stacks[s_index], s);
                const old_cost = self.energy_cost;
                self.energy_cost += s_cost + h_cost;
                self.hallway[i] = EMPTY;
                const total_cost = self.compute_minimal_energy();
                self.hallway[i] = s;
                _ = self.stacks[s_index].pop();
                self.energy_cost = old_cost;
                return total_cost;
            }
        }

        for (self.stacks[0..], 0..) |*stack, i| {
            if (self.stack_occupiable(i)) {
                continue;
            }
            const s_col = stack_to_column(i);
            const c_species = stack.items[stack.items.len - 1];
            if (self.can_go_home(c_species, s_col)) {
                const t_index = species_to_target_stack(c_species);
                const t_col = stack_to_column(t_index);
                const s_from_cost = self.stack_cost(stack, c_species);
                const h_cost = hallway_cost(c_species, s_col, t_col);
                try self.stacks[t_index].append(stack.pop());
                const s_to_cost = self.stack_cost(&self.stacks[t_index], c_species);
                const old_cost = self.energy_cost;
                self.energy_cost += s_from_cost + h_cost + s_to_cost;
                const total_cost = self.compute_minimal_energy();
                try stack.append(self.stacks[t_index].pop());
                self.energy_cost = old_cost;
                return total_cost;
            }
        }

        var total_cost: u64 = std.math.maxInt(u64);
        for (self.stacks[0..], 0..) |*stack, i| {
            if (self.stack_occupiable(i)) {
                continue;
            }
            const s_species = stack.items[stack.items.len - 1];
            const s_col = stack_to_column(i);
            const s_cost = self.stack_cost(stack, s_species);
            _ = stack.pop();
            var col: isize = @intCast(s_col);
            while (col < self.hallway.len and self.hallway[@as(usize, @intCast(col))] == EMPTY) : (col += 1) {
                const c_col: usize = @intCast(col);
                if (!is_home_column(c_col)) {
                    const h_cost = hallway_cost(s_species, s_col, c_col);
                    const old_cost = self.energy_cost;
                    self.energy_cost += s_cost + h_cost;
                    self.hallway[c_col] = s_species;
                    const t_cost = try self.compute_minimal_energy();
                    if (t_cost < total_cost) {
                        total_cost = t_cost;
                    }
                    self.hallway[c_col] = EMPTY;
                    self.energy_cost = old_cost;
                }
            }
            col = @intCast(s_col);
            while (col >= 0 and self.hallway[@as(usize, @intCast(col))] == EMPTY) : (col -= 1) {
                const c_col: usize = @intCast(col);
                if (!is_home_column(c_col)) {
                    const h_cost = hallway_cost(s_species, s_col, c_col);
                    const old_cost = self.energy_cost;
                    self.energy_cost += s_cost + h_cost;
                    self.hallway[c_col] = s_species;
                    const t_cost = try self.compute_minimal_energy();
                    if (t_cost < total_cost) {
                        total_cost = t_cost;
                    }
                    self.hallway[c_col] = EMPTY;
                    self.energy_cost = old_cost;
                }
            }
            try stack.append(s_species);
        }

        return total_cost;
    }

    fn init(a: std.mem.Allocator) Board {
        var ret: Board = .{
            .hallway = .{EMPTY} ** 11,
            .stacks = undefined,
            .max_stack_len = 2,
            .energy_cost = 0,
            .explored = std.AutoHashMap([2]u64, u64).init(a),
        };
        for (0..ret.stacks.len) |i| {
            ret.stacks[i] = std.ArrayList(u8).init(a);
        }
        return ret;
    }

    fn deinit(self: *Board) void {
        self.explored.deinit();
        for (self.stacks) |stack| {
            stack.deinit();
        }
    }
};

fn parse(input: []const u8) !Board {
    var board = Board.init(allocator);
    var iter = std.mem.splitSequence(u8, input, os_delimiter);
    while (iter.next()) |row_slice| {
        var stack_index: usize = 0;
        for (row_slice) |c| {
            if (c == 'A' or c == 'B' or c == 'C' or c == 'D') {
                try board.stacks[stack_index].insert(0, c);
                stack_index += 1;
            }
        }
    }
    return board;
}

fn create_part2_input(part1_input: Board) !Board {
    var ret = Board.init(allocator);
    ret.max_stack_len = 4;
    const row1 = [_]u8{ 'D', 'C', 'B', 'A' };
    const row2 = [_]u8{ 'D', 'B', 'A', 'C' };
    for (ret.stacks[0..], 0..) |*stack, i| {
        try stack.append(part1_input.stacks[i].items[0]);
        try stack.append(row2[i]);
        try stack.append(row1[i]);
        try stack.append(part1_input.stacks[i].items[1]);
    }
    return ret;
}

pub fn main() !void {
    const writer = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    var part1_input = try parse(content);
    defer part1_input.deinit();
    const result1 = try part1_input.compute_minimal_energy();
    try writer.print("Part 1: {d}\n", .{result1});
    var part2_input = try create_part2_input(part1_input);
    defer part2_input.deinit();
    const result2 = try part2_input.compute_minimal_energy();
    try writer.print("Part 2: {d}\n", .{result2});
}
