const std = @import("std");
const lib = @import("lib.zig");
const cwd = std.fs.cwd;
const Allocator = std.mem.Allocator;
const Token = lib.Token;
const Ast = lib.Ast;

fn preprocess(allocator: Allocator, input: []const u8) Allocator.Error![]const u8 {
    var output = try allocator.alloc(u8, 0);
    var i: usize = 0;

    while (i < input.len) {
        while (std.mem.startsWith(u8, input[i..], "//"))
            while (i < input.len and input[i] != '\n') : (i += 1) {};

        const start = i;
        while (i < input.len and input[i] != '\n') : (i += 1) {}
        if (i != input.len) i += 1;

        const len = i - start;
        output = try allocator.realloc(output, output.len + len);
        @memcpy(output[output.len - len ..], input[start..i]);
    }

    return output;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) @panic("Memory leak detected!");

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = try cwd().readFileAlloc(allocator, "main.con", std.math.maxInt(usize));
    const processed = try preprocess(allocator, input);

    const tokens = try Token.parse(allocator, processed);
    const asts = try Ast.parse(allocator, tokens);
    for (asts) |ast|
        ast.print(0);
}
