const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const Context = lib.Context;
const Token = lib.Token;
const Type = lib.Type;
const tools = lib.tools;

const parser = lib.parser;
const Parser = parser.Parser;
const alt = parser.alt;
const tag = parser.tag;
const map = parser.map;
const mapWith = parser.mapWith;
const drain = parser.drain;
const many0 = parser.many0;
const delimited0 = parser.delimited0;
const sequence = parser.sequence;

pub const Ast = union(enum) {
    const Param_t = struct {
        name: []const u8,
        ty: Type,
    };

    const Function_t = struct {
        name: []const u8,
        params: []const Param_t,
        exprs: []Ast,
        ret: Type,
    };

    const Call_t = struct {
        f: *Ast,
        exprs: []Ast,
    };

    const BinOpKind = enum {
        Add,
    };

    const BinOp_t = struct {
        kind: BinOpKind,
        lhs: *Ast,
        rhs: *Ast,
    };

    Integer: usize,
    Identifier: []const u8,
    Call: Call_t,
    Function: Function_t,
    BinOp: BinOp_t,
    Return: *Ast,

    pub fn parse(allocator: Allocator, input: []const Token) Allocator.Error![]Ast {
        const result = try many0(
            Ast.p_function(),
            null,
        ).parse(allocator, input) orelse @panic("Couldn't parse");
        return result.output;
    }

    fn p_expr() Parser(Token, Ast) {
        const gen = struct {
            fn f(allocator: Allocator, input: []const Token) Parser(Token, Ast).Result {
                if (try Ast.p_binOp().parse(allocator, input)) |result|
                    return result;
                if (try Ast.p_return().parse(allocator, input)) |result|
                    return result;
                if (try Ast.p_call().parse(allocator, input)) |result|
                    return result;
                if (try Ast.p_int().parse(allocator, input)) |result|
                    return result;
                return null;
            }
        };

        return .{ ._parse = gen.f };
    }

    fn p_stmt() Parser(Token, Ast) {
        return map(
            sequence(.{
                Ast.p_expr(),
                drain(tag(Token, .SemiColon)),
            }),
            tools.index(Ast, 0),
        );
    }

    fn p_int() Parser(Token, Ast) {
        return map(
            map(tag(Token, .Integer), tools.unTag(Token, usize, .Integer)),
            Ast.fromInt,
        );
    }

    fn p_word() Parser(Token, Ast) {
        return map(
            map(tag(Token, .Word), tools.unTag(Token, []const u8, .Word)),
            Ast.fromWord,
        );
    }

    fn p_return() Parser(Token, Ast) {
        return mapWith(
            sequence(.{
                drain(tag(Token, .Return)),
                p_expr(),
            }),
            fromReturn,
        );
    }

    fn p_binOp() Parser(Token, Ast) {
        return mapWith(
            sequence(.{
                alt(.{
                    tag(Token, .Add),
                }),
                p_expr(),
                p_expr(),
            }),
            fromBinOp,
        );
    }

    fn p_call() Parser(Token, Ast) {
        const gen = struct {
            fn f(allocator: Allocator, _input: []const Token) Parser(Token, Ast).Result {
                const word = try p_word().parse(allocator, _input) orelse return null;
                var input = word.input;
                var output = word.output;

                while (try p_tuple().parse(allocator, input)) |result| {
                    input = result.input;

                    output = .{ .Call = .{
                        .f = try tools.box(allocator, output),
                        .exprs = result.output,
                    } };
                }

                return .{ .input = input, .output = output };
            }
        };

        return .{ ._parse = gen.f };
    }

    fn p_tuple() Parser(Token, []Ast) {
        return map(
            sequence(.{
                drain(tag(Token, .LParen)),
                delimited0(Ast.p_expr(), tag(Token, .Comma)),
                drain(tag(Token, .RParen)),
            }),
            tools.index([]Ast, 1),
        );
    }

    fn p_param() Parser(Token, Param_t) {
        return map(
            sequence(.{
                map(tag(Token, .Type), tools.unTag(Token, Type, .Type)),
                map(tag(Token, .Word), tools.unTag(Token, []const u8, .Word)),
            }),
            fromParam,
        );
    }

    fn p_params() Parser(Token, []Param_t) {
        return map(
            sequence(.{
                drain(tag(Token, .LParen)),
                delimited0(
                    Ast.p_param(),
                    tag(Token, .Comma)
                ),
                drain(tag(Token, .RParen)),
            }),
            tools.index([]Param_t, 1),
        );
    }

    fn p_block() Parser(Token, []Ast) {
        return map(
            sequence(.{
                drain(tag(Token, .LBracket)),
                many0(Ast.p_stmt(), null),
                drain(tag(Token, .RBracket)),
            }),
            tools.index([]Ast, 1),
        );
    }

    fn p_function() Parser(Token, Ast) {
        const gen = struct {
            fn f(allocator: Allocator, input: []const Token) Parser(Token, Ast).Result {
                return map(
                    sequence(.{
                        map(tag(Token, .Type), tools.unTag(Token, Type, .Type)),
                        map(tag(Token, .Word), tools.unTag(Token, []const u8, .Word)),
                        Ast.p_params(),
                        Ast.p_block(),
                    }),
                    fromFunction,
                ).parse(allocator, input);
            }
        };

        return .{ ._parse = gen.f };
    }

    fn fromInt(source: usize) Ast {
        return .{ .Integer = source };
    }

    fn fromWord(source: []const u8) Ast {
        return .{ .Identifier = source };
    }

    fn fromParam(source: struct { Type, []const u8 }) Param_t {
        return .{ .name = source[1], .ty = source[0] };
    }

    fn fromReturn(allocator: Allocator, source: struct { void, Ast }) Allocator.Error!Ast {
        return .{ .Return = try tools.box(allocator, source[1]) };
    }

    fn fromBinOp(allocator: Allocator, source: struct { Token, Ast, Ast }) Allocator.Error!Ast {
        return .{ .BinOp = .{
            .kind = switch (source[0]) {
                .Add => .Add,
                else => unreachable,
            },
            .lhs = try tools.box(allocator, source[1]),
            .rhs = try tools.box(allocator, source[2]),
        } };
    }

    fn fromFunction(source: struct { Type, []const u8, []Param_t, []Ast }) Ast {
        return .{ .Function = .{
            .name = source[1],
            .params = source[2],
            .exprs = source[3],
            .ret = source[0],
        } };
    }

    // Debug only
    pub fn print(self: Ast, level: usize) void {
        const p = std.debug.print;

        switch (self) {
            .Integer => |int| {
                for (0..level) |_| p("    ", .{});
                p("{d}\n", .{int});
            },
            .Identifier => |ident| {
                for (0..level) |_| p("    ", .{});
                p("{s}\n", .{ident});
            },
            .Call => |tuple| {
                for (0..level) |_| p("    ", .{});
                p("Call:\n", .{});
                tuple.f.print(level + 1);
                for (tuple.exprs) |expr|
                    expr.print(level + 1);
            },
            .Function => |tuple| {
                for (0..level) |_| p("    ", .{});
                p("Function: {s}\n", .{tuple.name});
                for (tuple.exprs) |expr|
                    expr.print(level + 1);
            },
            .BinOp => |tuple| {
                for (0..level) |_| p("    ", .{});
                p("BinOp: {s}\n", .{@tagName(tuple.kind)});
                tuple.lhs.print(level + 1);
                tuple.rhs.print(level + 1);
            },
            .Return => |value| {
                for (0..level) |_| p("    ", .{});
                p("Return\n", .{});
                value.print(level + 1);
            },
        }
    }
};
