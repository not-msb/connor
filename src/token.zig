const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const Type = lib.Type;
const tools = lib.tools;

const parser = lib.parser;
const Parser = parser.Parser;
const byte = parser.byte;
const bytes = parser.bytes;
const alt = parser.alt;
const map = parser.map;
const replace = parser.replace;
const takeWhile1 = parser.takeWhile1;
const many0 = parser.many0;

pub const Token = union(enum) {
    Add,
    LParen,
    RParen,
    LBracket,
    RBracket,
    SemiColon,
    Comma,
    Return,
    Export,
    Type: Type,
    Integer: usize,
    Word: []const u8,

    pub fn parse(allocator: Allocator, input: []const u8) Allocator.Error![]Token {
        const r = try many0(
            alt(.{
                parseBinOp(),
                replace(byte(u8, '('), @as(Token, .LParen)),
                replace(byte(u8, ')'), @as(Token, .RParen)),
                replace(byte(u8, '{'), @as(Token, .LBracket)),
                replace(byte(u8, '}'), @as(Token, .RBracket)),
                replace(byte(u8, ';'), @as(Token, .SemiColon)),
                replace(byte(u8, ','), @as(Token, .Comma)),
                replace(bytes(u8, "return"), @as(Token, .Return)),
                replace(bytes(u8, "export"), @as(Token, .Export)),
                parseType(),
                map(takeWhile1(u8, std.ascii.isDigit), fromInt),
                map(takeIdentifier(), fromWord),
            }),
            std.ascii.isWhitespace,
        ).parse(allocator, input);

        const res = if (r) |res| res else @panic("Couldn't tokenize");
        return res.output;
    }

    fn parseType() Parser(u8, Token) {
        return alt(.{
            replace(bytes(u8, "u8"), Token{ .Type = .U8 }),
            replace(bytes(u8, "u32"), Token{ .Type = .U32 }),
        });
    }

    fn parseBinOp() Parser(u8, Token) {
        return alt(.{
            replace(byte(u8, '+'), @as(Token, .Add)),
        });
    }

    fn takeIdentifier() Parser(u8, []const u8) {
        const gen = struct {
            fn f(allocator: Allocator, input: []const u8) Parser(u8, []const u8).Result {
                _ = allocator;
                var i: usize = 0;
                while (i < input.len and (std.ascii.isAlphabetic(input[i]) or input[i] == '_')) : (i += 1) {}
                while (i < input.len and (std.ascii.isAlphabetic(input[i]) or std.ascii.isDigit(input[i]) or input[i] == '_')) : (i += 1) {}
                return .{
                    .input = input[i..],
                    .output = input[0..i],
                };
            }
        };

        return .{ ._parse = gen.f };
    }

    fn fromInt(input: []const u8) Token {
        return .{ .Integer = std.fmt.parseInt(usize, input, 10) catch unreachable };
    }

    fn fromWord(input: []const u8) Token {
        return .{ .Word = input };
    }
};
