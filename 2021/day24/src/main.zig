const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const Instruction = struct {
    instr: []const u8,
    op1: u8,
    op2: ?[]const u8,

    fn apply(self: *const Instruction, alu: *ALU) !void {
        if (std.mem.eql(u8, self.instr, "inp")) {
            const val = alu.inputs.items[alu.input_index];
            const reg = self.get_first_operand(alu);
            reg.* = val;
            alu.input_index += 1;
        } else if (std.mem.eql(u8, self.instr, "add")) {
            const op1 = self.get_first_operand(alu);
            const op2 = try self.get_second_operand(alu);
            op1.* += op2;
        } else if (std.mem.eql(u8, self.instr, "mul")) {
            const op1 = self.get_first_operand(alu);
            const op2 = try self.get_second_operand(alu);
            op1.* *= op2;
        } else if (std.mem.eql(u8, self.instr, "div")) {
            const op1 = self.get_first_operand(alu);
            const op2 = try self.get_second_operand(alu);
            op1.* = @divTrunc(op1.*, op2);
        } else if (std.mem.eql(u8, self.instr, "mod")) {
            const op1 = self.get_first_operand(alu);
            const op2 = try self.get_second_operand(alu);
            op1.* = @mod(op1.*, op2);
        } else if (std.mem.eql(u8, self.instr, "eql")) {
            const op1 = self.get_first_operand(alu);
            const op2 = try self.get_second_operand(alu);
            op1.* = if (op1.* == op2) 1 else 0;
        } else {
            unreachable;
        }
        alu.instruction_index += 1;
    }

    fn get_first_operand(self: *const Instruction, alu: *ALU) *i64 {
        return if (self.op1 == 'w')
            &alu.w
        else if (self.op1 == 'x')
            &alu.x
        else if (self.op1 == 'y')
            &alu.y
        else if (self.op1 == 'z')
            &alu.z
        else
            unreachable;
    }

    fn get_second_operand(self: *const Instruction, alu: *ALU) !i64 {
        if (self.op2.?.len == 1 and self.op2.?[0] > '9') {
            return if (self.op2.?[0] == 'w')
                alu.w
            else if (self.op2.?[0] == 'x')
                alu.x
            else if (self.op2.?[0] == 'y')
                alu.y
            else if (self.op2.?[0] == 'z')
                alu.z
            else
                unreachable;
        }
        return std.fmt.parseInt(i64, self.op2.?, 10);
    }

    fn parse(input: []const u8) Instruction {
        var ret: Instruction = undefined;
        var iter = std.mem.splitScalar(u8, input, ' ');
        ret.instr = iter.next().?;
        ret.op1 = iter.next().?[0];
        ret.op2 = iter.next();
        return ret;
    }
};

const ALU = struct {
    w: i64,
    x: i64,
    y: i64,
    z: i64,
    instructions: std.ArrayList(Instruction),
    instruction_index: usize,
    inputs: std.ArrayList(i64),
    input_index: usize,

    fn init(a: std.mem.Allocator) ALU {
        return ALU{
            .w = 0,
            .x = 0,
            .y = 0,
            .z = 0,
            .instructions = std.ArrayList(Instruction).init(a),
            .instruction_index = 0,
            .inputs = std.ArrayList(i64).init(a),
            .input_index = 0,
        };
    }

    fn deinit(self: *ALU) void {
        self.instructions.deinit();
        self.inputs.deinit();
    }

    fn run_until(self: *ALU, index: usize) !void {
        self.reset();
        for (0..index) |i| {
            const instruction = self.instructions.items[i];
            try instruction.apply(self);
        }
    }

    fn run(self: *ALU) !void {
        try self.run_until(self.instructions.items.len);
    }

    fn reset(self: *ALU) void {
        self.w = 0;
        self.x = 0;
        self.y = 0;
        self.z = 0;
        self.instruction_index = 0;
        self.input_index = 0;
    }

    fn try_code(self: *ALU, code: i64) !bool {
        while (self.inputs.items.len < 14) {
            try self.inputs.append(0);
        }
        var tmp = code;
        var index: isize = 13;
        while (tmp > 0) : (index -= 1) {
            self.inputs.items[@as(usize, @intCast(index))] = @mod(tmp, 10);
            tmp = @divTrunc(tmp, 10);
        }
        while (index > 0) : (index -= 1) {
            self.inputs.items[@as(usize, @intCast(index))] = 0;
        }
        try self.run();
        return self.z == 0;
    }
};

fn parse(input: []const u8) !ALU {
    var ret = ALU.init(allocator);
    var iter = std.mem.splitSequence(u8, input, os_delimiter);
    while (iter.next()) |instruction| {
        try ret.instructions.append(Instruction.parse(instruction));
    }
    return ret;
}

// Code is manually reverse-engineered
// Hint 1: For each 'inp w', there is either 'div z 1' or 'div z 26'
// Hint 2: There are seven 'div z 26' and seven 'div z 1'
// Hint 3: Modulo arithmetic, inspect z in base 26
pub fn main() !void {
    const writer = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    var alu = try parse(content);
    defer alu.deinit();
    const biggest_serial_number = 96918996924991;
    if (try alu.try_code(biggest_serial_number)) {
        try writer.print("Part 1: {d}\n", .{biggest_serial_number});
    }
    const smallest_serial_number = 91811241911641;
    if (try alu.try_code(smallest_serial_number)) {
        try writer.print("Part 2: {d}\n", .{smallest_serial_number});
    }
}
