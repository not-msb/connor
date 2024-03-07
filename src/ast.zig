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
const drain = parser.drain;
const many0 = parser.many0;
const sequence = parser.sequence;

const Node = union(enum) {
    const Function_t = struct {
        name: []const u8,
        exprs: []Ast,
    };

    const Call_t = struct {
        f: *Ast,
        expr: *Ast,
    };

    Integer: usize,
    Identifier: []const u8,
    Call: Call_t,
    Function: Function_t,
};

pub const Ast = struct {
    node: Node,
    ty: Type,

    const Function_t = struct { Type, []const u8, void, void, []Ast };

    pub fn parse(allocator: Allocator, input: []const Token) Allocator.Error!Ast {
        var context = Context.init(allocator);
        defer context.deinit();

        try context.put("add_u8", .{ .Function = .{
            .params = &[_]Type{.U8},
            .ret = &Type{ .Function = .{
                .params = &[_]Type{.U8},
                .ret = &.U8,
            } },
        } });

        const r = try Ast.p_function(&context, input);
        const res = if (r) |res| res else @panic("Couldn't parse");
        return res.output;
    }

    fn p_expr() Parser(Token, Ast, *Context) {
        const gen = struct {
            fn f(state: *Context, input: []const Token) Parser(Token, Ast, *Context).Result {
                if (try Ast.p_call().parse(state, input)) |result|
                    return result;
                if (try Ast.p_int().parse(state, input)) |result|
                    return result;
                if (try Ast.p_word().parse(state, input)) |result|
                    return result;
                return null;
            }
        };

        return .{ ._parse = gen.f };
    }

    fn p_stmt() Parser(Token, Ast, *Context) {
        return map(
            sequence(.{
                Ast.p_expr(),
                drain(tag(Token, *Context, .SemiColon)),
            }),
            tools.index(Ast, *Context, 0),
        );
    }

    fn p_int() Parser(Token, Ast, *Context) {
        return map(
            map(tag(Token, *Context, .Integer), tools.unTag(Token, usize, *Context, .Integer)),
            Ast.fromInt,
        );
    }

    fn p_word() Parser(Token, Ast, *Context) {
        return map(
            map(tag(Token, *Context, .Word), tools.unTag(Token, []const u8, *Context, .Word)),
            Ast.fromWord,
        );
    }

    fn p_call() Parser(Token, Ast, *Context) {
        const gen = struct {
            fn f(state: *Context, _input: []const Token) Parser(Token, Ast, *Context).Result {
                var input = _input;
                var output: Ast = undefined;

                if (try p_word().parse(state, input)) |result| {
                    input = result.input;
                    output = result.output;
                } else return null;

                while (output.ty == .Function) {
                    const result = try p_expr().parse(state, input) orelse break;
                    input = result.input;

                    output = .{
                        .node = .{ .Call = .{
                            .f = try tools.box(state.allocator, output),
                            .expr = try tools.box(state.allocator, result.output),
                        } },
                        .ty = output.ty.Function.ret.*,
                    };
                }

                return .{ .input = input, .output = output };
            }
        };

        return .{ ._parse = gen.f };
    }

    fn p_block() Parser(Token, []Ast, *Context) {
        return map(
            sequence(.{
                drain(tag(Token, *Context, .LBracket)),
                many0(Ast.p_stmt(), null),
                drain(tag(Token, *Context, .RBracket)),
            }),
            tools.index([]Ast, *Context, 1),
        );
    }

    fn p_function(state: *Context, _input: []const Token) Parser(Token, Ast, *Context).Result {
        var input = _input;

        const prefix = try sequence(.{
            map(tag(Token, void, .Type), tools.unTag(Token, Type, void, .Type)),
            map(tag(Token, void, .Word), tools.unTag(Token, []const u8, void, .Word)),
            drain(tag(Token, void, .LParen)),
            drain(tag(Token, void, .RParen)),
        }).parse(undefined, input) orelse return null;

        input = prefix.input;
        const output = prefix.output;

        var context = try state.clone();
        defer context.deinit();

        try context.put("return", .{ .Function = .{
            .params = &[1]Type{output[0]},
            .ret = &.NoReturn,
        } });

        const block = try Ast.p_block().parse(&context, input) orelse return null;
        for (block.output) |*expr|
            expr.unComp(null);
        const ast = try Ast.fromFunction(state, .{ output[0], output[1], output[2], output[3], block.output });
        ast.check();

        return .{
            .input = block.input,
            .output = ast,
        };
    }

    pub fn check(self: Ast) void {
        return switch (self.node) {
            .Integer, .Identifier => {},
            .Call => |tuple| {
                tuple.f.check();
                tuple.expr.check();

                const params = tuple.f.ty.Function.params;
                const ty = tuple.expr.ty;

                if (!ty.coercible(.{ .Tuple = params })) @panic("Incompatible types in call");
            },
            .Function => |tuple| for (tuple.exprs) |expr|
                expr.check(),
        };
    }

    pub fn unComp(self: *Ast, expected: ?Type) void {
        switch (self.node) {
            .Integer, .Identifier => {
                if (expected) |ty| self.ty = ty;
            },
            .Call => |tuple| {
                const params = tuple.f.ty.Function.params;
                tuple.expr.unComp(if (params.len == 0) null else params[0]);
                tuple.f.unComp(null);
            },
            .Function => |tuple| for (0..tuple.exprs.len) |i|
                tuple.exprs[i].unComp(null),
        }
    }

    fn fromInt(state: *Context, source: usize) Context.Error!Ast {
        _ = state;
        return .{
            .node = .{ .Integer = source },
            .ty = .CompInt,
        };
    }

    fn fromWord(state: *Context, source: []const u8) Context.Error!Ast {
        return .{
            .node = .{ .Identifier = source },
            .ty = state.get(source).?,
        };
    }

    fn fromFunction(ctx: *Context, source: Function_t) Context.Error!Ast {
        const ty = .{ .Function = .{
            .params = try ctx.allocator.alloc(Type, 0),
            .ret = try tools.box(ctx.allocator, source[0]),
        } };
        try ctx.put(source[1], ty);

        return .{
            .node = .{ .Function = .{
                .name = source[1],
                .exprs = source[4],
            } },
            .ty = ty,
        };
    }

    // Debug only
    pub fn print(self: Ast, level: usize) void {
        const p = std.debug.print;

        switch (self.node) {
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
                tuple.expr.print(level + 1);
            },
            .Function => |tuple| {
                for (0..level) |_| p("    ", .{});
                p("Function: {s}\n", .{tuple.name});
                for (tuple.exprs) |expr|
                    expr.print(level + 1);
            },
        }

        //for (0..level) |_| p("    ", .{});
        //std.debug.print("Type: {any}\n", .{self.ty});
    }
};
