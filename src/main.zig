const std = @import("std");
const lib = @import("lib.zig");
const cwd = std.fs.cwd;
const Allocator = std.mem.Allocator;
const Token = lib.Token;
const Ast = lib.Ast;
const State = @import("ir.zig").State;
const Ir = @import("ir.zig").Ir;
const Context = lib.Context;
const append = lib.tools.append;

fn preprocess(allocator: Allocator, input: []const u8) Allocator.Error![]const u8 {
    var output = try allocator.alloc(u8, 0);
    var i: usize = 0;

    while (i < input.len) {
        const start = i;
        while (i < input.len and !std.mem.startsWith(u8, input[i..], "//")) : (i += 1) {}
        const len = i - start;

        while (i < input.len and input[i] != '\n') : (i += 1) {}
        output = try append(u8, allocator, output, input[start..start+len]);
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

    var context = Context.init(allocator);
    for (asts) |ast|
        try context.check(ast, null);

    const irs = try Ir.from(allocator, asts);
    var state = State.init(allocator);
    for (irs) |ir|
        _ = try ir.compile(&state);
}
