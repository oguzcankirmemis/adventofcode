const std = @import("std");

pub fn main() !void {
    const help_message =
        \\Welcome to Advent of Code 2021 Solutions in Zig!
        \\
        \\Use 'zig build run -Dday=[day]' -Dinput=[input_path] to run a solution of a given day against an input.
        \\The default input is located at [day]/inputs/example.txt.
        \\
        \\Day 0 is used for debugging purposes.
    ;
    const stdout = std.io.getStdOut();
    try stdout.writer().print("{s}", .{help_message});
}
