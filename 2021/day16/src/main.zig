const std = @import("std");
const config = @import("config");
const builtin = @import("builtin");

const MAX_FILE_SIZE = 1024 * 1024;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const input_path = config.input_path;
const os_delimiter = if (builtin.os.tag == .windows) "\r\n" else "\n";

const Packet = struct {
    version: u64,
    packet_type: u64,
    is_literal: bool,
    literal: u64,
    is_length_type_0: bool,
    total_length: u64,
    is_length_type_1: bool,
    number_of_packets: u64,
};

fn parseVersion(packet: std.ArrayList(u8), cursor: *usize) u64 {
    var version: u64 = 0;
    for (0..3) |i| {
        version = (version << 1) + (packet.items[cursor.* + i] - '0');
    }
    cursor.* += 3;
    return version;
}

fn parseType(packet: std.ArrayList(u8), cursor: *usize) u64 {
    var packet_type: u64 = 0;
    for (0..3) |i| {
        packet_type = (packet_type << 1) + (packet.items[cursor.* + i] - '0');
    }
    cursor.* += 3;
    return packet_type;
}

fn parseLiteralValue(packet: std.ArrayList(u8), cursor: *usize) u64 {
    var literal: u64 = 0;
    while (packet.items[cursor.*] != '0') {
        for (1..5) |i| {
            literal = (literal << 1) + (packet.items[cursor.* + i] - '0');
        }
        cursor.* += 5;
    }
    for (1..5) |i| {
        literal = (literal << 1) + (packet.items[cursor.* + i] - '0');
    }
    cursor.* += 5;
    return literal;
}

fn parseLengthType(packet: std.ArrayList(u8), cursor: *usize) u64 {
    const length_type = packet.items[cursor.*] - '0';
    cursor.* += 1;
    return length_type;
}

fn parseTotalLength(packet: std.ArrayList(u8), cursor: *usize) u64 {
    var length: u64 = 0;
    for (0..15) |i| {
        length = (length << 1) + (packet.items[cursor.* + i] - '0');
    }
    cursor.* += 15;
    return length;
}

fn parseNumberOfPackets(packet: std.ArrayList(u8), cursor: *usize) u64 {
    var number_of_packets: usize = 0;
    for (0..11) |i| {
        number_of_packets = (number_of_packets << 1) + (packet.items[cursor.* + i] - '0');
    }
    cursor.* += 11;
    return number_of_packets;
}

fn parsePacket(packet: std.ArrayList(u8), cursor: *usize) Packet {
    const version = parseVersion(packet, cursor);
    const packet_type = parseType(packet, cursor);
    var ret = Packet{
        .version = version,
        .packet_type = packet_type,
        .is_literal = false,
        .literal = 0,
        .is_length_type_0 = false,
        .total_length = 0,
        .is_length_type_1 = false,
        .number_of_packets = 0,
    };
    if (packet_type == 4) {
        ret.is_literal = true;
        ret.literal = parseLiteralValue(packet, cursor);
        return ret;
    }
    const length_type = parseLengthType(packet, cursor);
    if (length_type == 0) {
        ret.is_length_type_0 = true;
        ret.total_length = parseTotalLength(packet, cursor);
    } else {
        ret.is_length_type_1 = true;
        ret.number_of_packets = parseNumberOfPackets(packet, cursor);
    }
    return ret;
}

fn sumPacketVersions(packet: std.ArrayList(u8), cursor: *usize) u64 {
    const parsed_packet = parsePacket(packet, cursor);
    if (parsed_packet.is_literal) {
        return parsed_packet.version;
    }
    var packet_sum = parsed_packet.version;
    if (parsed_packet.is_length_type_0) {
        const end: u64 = cursor.* + parsed_packet.total_length;
        while (cursor.* != end) {
            packet_sum += sumPacketVersions(packet, cursor);
        }
        return packet_sum;
    }
    if (parsed_packet.is_length_type_1) {
        var i: u64 = 0;
        while (i < parsed_packet.number_of_packets) : (i += 1) {
            packet_sum += sumPacketVersions(packet, cursor);
        }
        return packet_sum;
    }
    unreachable;
}

fn sum(acc: u64, curr: u64) u64 {
    return acc + curr;
}

fn product(acc: u64, curr: u64) u64 {
    return acc * curr;
}

fn minimum(acc: u64, curr: u64) u64 {
    return if (acc < curr) acc else curr;
}

fn maximum(acc: u64, curr: u64) u64 {
    return if (acc > curr) acc else curr;
}

fn id(acc: u64, curr: u64) u64 {
    _ = curr;
    return acc;
}

fn greaterThan(acc: u64, curr: u64) u64 {
    return if (acc > curr) 1 else 0;
}

fn lessThan(acc: u64, curr: u64) u64 {
    return if (acc < curr) 1 else 0;
}

fn equal(acc: u64, curr: u64) u64 {
    return if (acc == curr) 1 else 0;
}

fn evaluatePacket(packet: std.ArrayList(u8), cursor: *usize) u64 {
    const parsed_packet = parsePacket(packet, cursor);
    if (parsed_packet.is_literal) {
        return parsed_packet.literal;
    }
    const aggregate: *const fn (acc: u64, curr: u64) u64 = switch (parsed_packet.packet_type) {
        0 => sum,
        1 => product,
        2 => minimum,
        3 => maximum,
        4 => id,
        5 => greaterThan,
        6 => lessThan,
        7 => equal,
        else => unreachable,
    };
    if (parsed_packet.is_length_type_0) {
        const end: usize = cursor.* + parsed_packet.total_length;
        var accumulated = evaluatePacket(packet, cursor);
        while (cursor.* != end) {
            accumulated = aggregate(accumulated, evaluatePacket(packet, cursor));
        }
        return accumulated;
    }
    if (parsed_packet.is_length_type_1) {
        var i: u64 = 1;
        var accumulated = evaluatePacket(packet, cursor);
        while (i < parsed_packet.number_of_packets) : (i += 1) {
            accumulated = aggregate(accumulated, evaluatePacket(packet, cursor));
        }
        return accumulated;
    }
    unreachable;
}

fn part1(packet: std.ArrayList(u8)) u64 {
    var cursor: u64 = 0;
    return sumPacketVersions(packet, &cursor);
}

fn part2(packet: std.ArrayList(u8)) u64 {
    var cursor: u64 = 0;
    return evaluatePacket(packet, &cursor);
}

fn parse(input: []const u8) !std.ArrayList(u8) {
    const lookup = [16][]const u8{ "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111" };
    var ret = std.ArrayList(u8).init(allocator);
    for (input) |c| {
        if (c < 'A') {
            try ret.appendSlice(lookup[c - '0']);
        } else {
            try ret.appendSlice(lookup[c - 'A' + 10]);
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
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    const file = try std.fs.cwd().openFile(input_path, .{});
    const content = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(content);
    const packet = try parse(content);
    defer packet.deinit();
    const result1 = part1(packet);
    try writer.print("Part 1: {d}\n", .{result1});
    const result2 = part2(packet);
    try writer.print("Part 2: {d}\n", .{result2});
}
