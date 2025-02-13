const std = @import("std");

const stdout = std.io.getStdOut().writer();
pub fn main() !void {
    const name = "Pedro";
    for (name, 0..) |_, i| {
        try stdout.print("{d} | ", .{i});
    }
}
