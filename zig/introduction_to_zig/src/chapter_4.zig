const std = @import("std");
const stdout = std.io.getStdOut().writer();

const Base64 = struct {
    _table: *const [64]u8,

    pub fn init() Base64 {
        const upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        const lower = "abcdefghijklmnopqrstuvwxyz";
        const digits = "0123456789";
        const symbols = "+/";
        // stdout.print("Base64 table: {}\n", upper ++ lower ++ digits ++ symbols) catch unreachable;
        return Base64{
            ._table = upper ++ lower ++ digits ++ symbols,
        };
    }

    pub fn _char_at(self: *const Base64, index: usize) u8 {
        return self._table[index];
    }

    pub fn encode(self: *const Base64, allocator: std.mem.Allocator, input: []const u8) ![]const u8 {
        var output_size = try std.math.divCeil(usize, input.len, 3);
        output_size *= 4;
        var output = try allocator.alloc(u8, output_size);

        var i: u16 = 0;
        while (i < input.len) : (i += 3) {
            const byte_1 = input[i];
            var byte_2: u8 = 0;
            var byte_3: u8 = 0;
            var char_3_active = false;
            var char_4_active = false;

            if (i < input.len - 1) {
                byte_2 = input[i + 1];
                char_3_active = true;
            }
            if (i < input.len - 2) {
                byte_3 = input[i + 2];
                char_4_active = true;
            }

            const char_1 = byte_1 >> 2;
            const char_2 = ((byte_1 & 0b00000011) << 4) | ((byte_2 & 0b11110000) >> 4);
            const char_3 = ((byte_2 & 0b00001111) << 2) | ((byte_3 & 0b11000000) >> 4);
            const char_4 = byte_3 & 0b00111111;
            var output_i = try std.math.divFloor(usize, i, 3);
            output_i *= 4;
            output[output_i] = self._char_at(char_1);
            output[output_i + 1] = self._char_at(char_2);
            if (char_3_active) {
                output[output_i + 2] = self._char_at(char_3);
            } else {
                output[output_i + 2] = '=';
            }
            if (char_4_active) {
                output[output_i + 3] = self._char_at(char_4);
            } else {
                output[output_i + 3] = '=';
            }
        }

        return output;
    }
};

pub fn main() !void {
    const base64 = Base64.init();
    try stdout.print("Character at index 28: {any}\n", .{base64._table.len});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    const output = try base64.encode(alloc, "Hi");
    try stdout.print("Hi: {s}", .{output});
}
