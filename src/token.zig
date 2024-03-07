const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const Type = lib.Type;
const tools = lib.tools;

const parser = lib.parser;
const byte = parser.byte;
const bytes = parser.bytes;
const alt = parser.alt;
const map = parser.map;
const replace = parser.replace;
const takeWhile1 = parser.takeWhile1;
const many0 = parser.many0;

pub const Token = union(enum) {
    LParen,
    RParen,
    LBracket,
    RBracket,
    SemiColon,
    Type: Type,
    Integer: usize,
    Word: []const u8,

    pub fn parse(allocator: Allocator, _input: []const u8) Allocator.Error![]Token {
        const r = try many0(
            alt(.{
                replace(byte(u8, Allocator, '('), @as(Token, .LParen)),
                replace(byte(u8, Allocator, ')'), @as(Token, .RParen)),
                replace(byte(u8, Allocator, '{'), @as(Token, .LBracket)),
                replace(byte(u8, Allocator, '}'), @as(Token, .RBracket)),
                replace(byte(u8, Allocator, ';'), @as(Token, .SemiColon)),
                replace(bytes(u8, Allocator, "u8"), Token{ .Type = .U8 }),
                map(takeWhile1(u8, Allocator, std.ascii.isDigit), fromInt),
                map(takeWhile1(u8, Allocator, std.ascii.isAlphabetic), fromWord),
            }),
            std.ascii.isWhitespace,
        ).parse(allocator, _input);

        const res = if (r) |res| res else @panic("Couldn't tokenize");
        return res.output;
    }

    fn fromInt(state: Allocator, input: []const u8) Allocator.Error!Token {
        _ = state;
        return .{ .Integer = std.fmt.parseInt(usize, input, 10) catch unreachable };
    }

    fn fromWord(state: Allocator, input: []const u8) Allocator.Error!Token {
        _ = state;
        return .{ .Word = input };
    }
};
