const std = @import("std");

const stdout = std.io.getStdOut().writer();
pub fn main() !void {
    var utf8 = try std.unicode.Utf8View.init("アメリカ");
    var iterator = utf8.iterator();
    while (iterator.nextCodepointSlice()) |codepoint| {
        try stdout.print(
            "got codepoint {}\n",
            .{std.fmt.fmtSliceHexUpper(codepoint)},
        );
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
