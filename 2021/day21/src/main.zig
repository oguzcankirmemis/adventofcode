const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");
const common = @import("common");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const Input = struct {
    player1: u64,
    player2: u64,
};

const DiracDiceUniverse = struct {
    dice_sum: u64,
    number_of_universes: u64,
};

const DiracSimulationReport = struct {
    player1_wins: u64,
    player2_wins: u64,
};

const DIRAC_POSSIBLE_OUTCOMES = [_]DiracDiceUniverse{
    .{ .dice_sum = 3, .number_of_universes = 1 },
    .{ .dice_sum = 4, .number_of_universes = 3 },
    .{ .dice_sum = 5, .number_of_universes = 6 },
    .{ .dice_sum = 6, .number_of_universes = 7 },
    .{ .dice_sum = 7, .number_of_universes = 6 },
    .{ .dice_sum = 8, .number_of_universes = 3 },
    .{ .dice_sum = 9, .number_of_universes = 1 },
};

fn parse(input: []const u8) !Input {
    var ret = Input{ .player1 = 0, .player2 = 0 };
    var iter = std.mem.splitSequence(u8, input, os_delimiter);
    const player1_str = iter.next().?;
    var i = player1_str.len - 1;
    var base: u64 = 1;
    while (player1_str[i] != ' ') : (i -= 1) {
        ret.player1 += base * (player1_str[i] - '0');
        base = base * 10;
    }
    const player2_str = iter.next().?;
    i = player2_str.len - 1;
    base = 1;
    while (player2_str[i] != ' ') : (i -= 1) {
        ret.player2 += base * (player2_str[i] - '0');
        base = base * 10;
    }
    return ret;
}

fn simulate_naive(player1: u64, player2: u64, dice: u64, target: u64) u64 {
    var p1 = player1;
    var p2 = player2;
    var s1: u64 = 0;
    var s2: u64 = 0;
    var r: u64 = 0;
    var d: u64 = 1;
    while (true) {
        p1 += d;
        d = if (d % dice == 0) 1 else d + 1;
        p1 += d;
        d = if (d % dice == 0) 1 else d + 1;
        p1 += d;
        d = if (d % dice == 0) 1 else d + 1;
        p1 = p1 % 10;
        s1 += p1 + 1;
        r += 3;
        if (s1 >= target) {
            return s2 * r;
        }
        p2 += d;
        d = if (d % dice == 0) 1 else d + 1;
        p2 += d;
        d = if (d % dice == 0) 1 else d + 1;
        p2 += d;
        d = if (d % dice == 0) 1 else d + 1;
        p2 = p2 % 10;
        s2 += p2 + 1;
        r += 3;
        if (s2 >= target) {
            return s1 * r;
        }
    }
    unreachable;
}

fn simulate_dirac(
    player1: u64,
    player2: u64,
    score1: u64,
    score2: u64,
    target: u64,
    turn_of_player1: bool,
    number_of_universes: u64,
    simulation_report: *DiracSimulationReport,
) void {
    if (score1 >= target) {
        simulation_report.player1_wins += number_of_universes;
        return;
    }
    if (score2 >= target) {
        simulation_report.player2_wins += number_of_universes;
        return;
    }
    for (DIRAC_POSSIBLE_OUTCOMES) |outcome| {
        if (turn_of_player1) {
            simulate_dirac(
                (player1 + outcome.dice_sum) % 10,
                player2,
                score1 + ((player1 + outcome.dice_sum) % 10) + 1,
                score2,
                target,
                false,
                number_of_universes * outcome.number_of_universes,
                simulation_report,
            );
        } else {
            simulate_dirac(
                player1,
                (player2 + outcome.dice_sum) % 10,
                score1,
                score2 + ((player2 + outcome.dice_sum) % 10) + 1,
                target,
                true,
                number_of_universes * outcome.number_of_universes,
                simulation_report,
            );
        }
    }
}

fn solve_part1(input: Input) u64 {
    return simulate_naive(input.player1 - 1, input.player2 - 1, 100, 1000);
}

fn solve_part2(input: Input) u64 {
    var report: DiracSimulationReport = .{
        .player1_wins = 0,
        .player2_wins = 0,
    };
    simulate_dirac(input.player1 - 1, input.player2 - 1, 0, 0, 21, true, 1, &report);
    return if (report.player1_wins > report.player2_wins) report.player1_wins else report.player2_wins;
}

pub fn main() !void {
    const writer = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const input = try parse(content);
    const result1 = solve_part1(input);
    try writer.print("Part 1: {d}\n", .{result1});
    const result2 = solve_part2(input);
    try writer.print("Part 2: {d}\n", .{result2});
}
