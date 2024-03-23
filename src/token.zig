const std = @import("std");
const lib = @import("lib.zig");
const panic = std.debug.panic;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Type = lib.Type;

pub const Token = union(enum) {
    Eof,
    Add,
    LParen,
    RParen,
    LBracket,
    RBracket,
    SemiColon,
    Comma,
    Return,
    Export,
    Extern,
    Type: Type,
    Integer: usize,
    Word: []const u8,
};

pub const Lexer = struct {
    allocator: Allocator,
    tokens: ArrayList(Token),

    pub fn parse(allocator: Allocator, input: []const u8) Allocator.Error!Lexer {
        var tokens = ArrayList(Token).init(allocator);
        var i: usize = 0;

        while (i < input.len) : (i += 1) {
            while (i < input.len and std.ascii.isWhitespace(input[i])) i += 1;
            if (i >= input.len) break;

            const token: Token = switch (input[i]) {
                '+' => .Add,
                '(' => .LParen,
                ')' => .RParen,
                '{' => .LBracket,
                '}' => .RBracket,
                ';' => .SemiColon,
                ',' => .Comma,
                else => b: {
                    const pairs = .{
                        .{ "return", .Return },
                        .{ "extern", .Extern },
                        .{ "export", .Export },
                        .{ "u8", .{ .Type = .U8 } },
                        .{ "u32", .{ .Type = .U32 } },
                        .{ "u64", .{ .Type = .U64 } },
                        .{ "noreturn", .{ .Type = .NoReturn } },
                    };

                    inline for (pairs) |pair|
                        if (std.mem.startsWith(u8, input[i..], pair[0])) {
                            i += pair[0].len - 1;
                            break :b pair[1];
                        };

                    if (takeInteger(input[i..])) |slice| {
                        i += slice.len - 1;
                        break :b .{ .Integer = std.fmt.parseInt(usize, slice, 10) catch unreachable };
                    }

                    if (takeIdentifier(input[i..])) |slice| {
                        i += slice.len - 1;
                        break :b .{ .Word = slice }; //TODO: Rename ident OR Word
                    }
                },
            };

            try tokens.append(token);
        }

        std.mem.reverse(Token, tokens.items);
        return .{
            .allocator = allocator,
            .tokens = tokens,
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit();
    }

    pub fn clone(self: *const Lexer) Allocator.Error!Lexer {
        return .{
            .allocator = self.allocator,
            .tokens = try self.tokens.clone(),
        };
    }

    fn takeInteger(input: []const u8) ?[]const u8 {
        var i: usize = 0;
        while (i < input.len and std.ascii.isDigit(input[i])) i += 1;
        return if (i == 0) null else input[0..i];
    }

    fn takeIdentifier(input: []const u8) ?[]const u8 {
        var i: usize = 0;
        while (i < input.len and (std.ascii.isAlphabetic(input[i]) or input[i] == '_')) i += 1;
        while (i < input.len and (std.ascii.isAlphabetic(input[i]) or std.ascii.isDigit(input[i]) or input[i] == '_')) i += 1;
        return if (i == 0) null else input[0..i];
    }

    pub fn next(self: *Lexer) Token {
        return self.tokens.popOrNull() orelse .Eof;
    }

    pub fn peek(self: *const Lexer) Token {
        return self.tokens.getLastOrNull() orelse .Eof;
    }
};
