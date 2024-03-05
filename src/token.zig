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
const take_while1 = parser.take_while1;

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
        var output = try allocator.alloc(Token, 0);
        var input = _input;

        while (input.len != 0) {
            while (input.len != 0 and std.ascii.isWhitespace(input[0])) input = input[1..];
            if (input.len == 0) break;

            const r = try alt(.{
                replace(byte(u8, '('), @as(Token, .LParen)),
                replace(byte(u8, ')'), @as(Token, .RParen)),
                replace(byte(u8, '{'), @as(Token, .LBracket)),
                replace(byte(u8, '}'), @as(Token, .RBracket)),
                replace(byte(u8, ';'), @as(Token, .SemiColon)),
                replace(bytes(u8, "u8"), Token{ .Type = .U8 }),
                map(null, take_while1(u8, std.ascii.isDigit), from_int),
                map(null, take_while1(u8, std.ascii.isAlphabetic), from_word),
            })(allocator, input);

            const res = if (r) |res| res else @panic("Couldn't tokenize");
            input = res.input;
            output = try tools.push(Token, allocator, output, res.output);
        }

        return output;
    }

    fn from_int(input: []const u8) Token {
        return .{ .Integer = std.fmt.parseInt(usize, input, 10) catch unreachable };
    }

    fn from_word(input: []const u8) Token {
        return .{ .Word = input };
    }
};
