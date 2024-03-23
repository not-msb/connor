const std = @import("std");
const lib = @import("lib.zig");
const cwd = std.fs.cwd;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Token = lib.Token;
const Lexer = lib.Lexer;
const File = lib.File;
const Ast = lib.Ast;
const Type = lib.Type;
const Storage = lib.Storage;
const IrFile = lib.IrFile;
const Ir = lib.Ir;
const Context = lib.Context;

fn preprocess(allocator: Allocator, input: []const u8) Allocator.Error![]const u8 {
    var output = ArrayList(u8).init(allocator);
    var i: usize = 0;

    while (i < input.len) {
        const start = i;
        while (i < input.len and !std.mem.startsWith(u8, input[i..], "//")) : (i += 1) {}
        const end = i;

        while (i < input.len and input[i] != '\n') : (i += 1) {}
        try output.appendSlice(input[start..end]);
    }

    return output.toOwnedSlice();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) @panic("Memory leak detected!");

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = try cwd().readFileAlloc(allocator, "main.con", std.math.maxInt(usize));
    const processed = try preprocess(allocator, input);

    const file = try File.parse(allocator, processed);
    try Context.scan(allocator, file);

    const ir_file = try IrFile.from(allocator, file);
    try ir_file.compile();

    std.debug.print("Arena:     {d}\n", .{arena.queryCapacity()});
    std.debug.print("Type:      {d}\n", .{@sizeOf(Type)});
    std.debug.print("Storage:   {d}\n", .{@sizeOf(Storage)});
    std.debug.print("Token:     {d}\n", .{@sizeOf(Token)});
    std.debug.print("Ast:       {d}\n", .{@sizeOf(Ast)});
    std.debug.print("Ir:        {d}\n", .{@sizeOf(Ir)});
}
