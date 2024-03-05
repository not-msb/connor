const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const Context = lib.Context;
const Token = lib.Token;
const Type = lib.Type;
const tools = lib.tools;

const parser = lib.parser;
const Result = parser.Result;
const tag = parser.tag;
const map = parser.map;
const drain = parser.drain;
const manyWith0 = parser.manyWith0;
const sequence = parser.sequence;

const Node = union(enum) {
    const Function_t = struct {
        name: []const u8,
        exprs: []Ast,
    };

    const Call_t = struct {
        name: []const u8,
        expr: *Ast,
    };

    Integer: usize,
    Identifier: []const u8,
    Call: Call_t,
    Function: Function_t,
};

pub const Ast = struct {
    node: Node,
    ty: ?Type,

    const Call_t = struct { []const u8, Ast };
    const Function_t = struct { Type, []const u8, void, void, []Ast };

    pub fn parse(allocator: Allocator, input: []const Token) Allocator.Error!void {
        var context = Context.init(allocator);
        defer context.deinit();

        const r = try Ast.p_function(allocator, input, &context);
        const res = if (r) |res| res else @panic("Couldn't parse");
        res.output.print(0);
    }

    fn p_expr(allocator: Allocator, input: []const Token, ctx: *Context) Allocator.Error!?Result(Token, Ast) {
        if (try Ast.p_call()(allocator, input, ctx)) |res|
            return res;
        if (try Ast.p_int()(allocator, input)) |res|
            return res;
        if (try Ast.p_word()(allocator, input, ctx)) |res|
            return res;
        return null;
    }

    fn p_stmt() fn (Allocator, []const Token, *Context) Allocator.Error!?Result(Token, Ast) {
        return map(
            *Context,
            sequence(*Context, .{
                Ast.p_expr,
                drain(tag(Token, .SemiColon)),
            }),
            tools.indexWith(Ast, *Context, 0),
        );
    }

    fn p_int() fn (Allocator, []const Token) Allocator.Error!?Result(Token, Ast) {
        return map(
            null,
            map(null, tag(Token, .Integer), tools.unTag(Token, usize, .Integer)),
            Ast.fromInt,
        );
    }

    fn p_word() fn (Allocator, []const Token, *Context) Allocator.Error!?Result(Token, Ast) {
        return map(
            *Context,
            map(null, tag(Token, .Word), tools.unTag(Token, []const u8, .Word)),
            Ast.fromWord,
        );
    }

    fn p_call() fn (Allocator, []const Token, *Context) Allocator.Error!?Result(Token, Ast) {
        return map(
            *Context,
            sequence(*Context, .{
                map(null, tag(Token, .Word), tools.unTag(Token, []const u8, .Word)),
                Ast.p_expr,
            }),
            Ast.fromCall,
        );
    }

    fn p_block() fn (Allocator, []const Token, *Context) Allocator.Error!?Result(Token, []Ast) {
        return map(
            *Context,
            sequence(*Context, .{
                drain(tag(Token, .LBracket)),
                manyWith0(*Context, Ast.p_stmt()),
                drain(tag(Token, .RBracket)),
            }),
            tools.indexWith([]Ast, *Context, 1),
        );
    }

    fn p_function(allocator: Allocator, _input: []const Token, ctx: *Context) Allocator.Error!?Result(Token, Ast) {
        var input = _input;

        const prefix = try sequence(null, .{
            map(null, tag(Token, .Type), tools.unTag(Token, Type, .Type)),
            map(null, tag(Token, .Word), tools.unTag(Token, []const u8, .Word)),
            drain(tag(Token, .LParen)),
            drain(tag(Token, .RParen)),
        })(allocator, input) orelse return null;

        input = prefix.input;
        const output = prefix.output;

        var context = try ctx.clone();
        defer context.deinit();

        try context.put("return", .{ .Function = .{
            .params = &[_]Type{output[0]},
            .ret = &.NoReturn,
        } });

        const block = try Ast.p_block()(allocator, input, &context) orelse return null;
        return .{
            .input = block.input,
            .output = try Ast.fromFunction(ctx, .{ output[0], output[1], output[2], output[3], block.output }),
        };
    }

    fn fromInt(source: usize) Ast {
        return .{
            .node = .{ .Integer = source },
            .ty = .CompInt,
        };
    }

    fn fromWord(source: []const u8, ctx: *Context) Ast {
        return .{
            .node = .{ .Identifier = source },
            .ty = ctx.get(source).?,
        };
    }

    fn fromCall(allocator: Allocator, source: Call_t, ctx: *Context) Allocator.Error!Ast {
        return .{
            .node = .{ .Call = .{
                .name = source[0],
                .expr = try tools.box(allocator, source[1]),
            } },
            .ty = ctx.get(source[0]).?.Function.ret.*,
        };
    }

    fn fromFunction(ctx: *Context, source: Function_t) Allocator.Error!Ast {
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
                p("Call: {s}\n", .{tuple.name});
                tuple.expr.print(level + 1);
            },
            .Function => |tuple| {
                for (0..level) |_| p("    ", .{});
                p("Function: {s}\n", .{tuple.name});
                for (tuple.exprs) |expr|
                    expr.print(level + 1);
            },
        }

        for (0..level) |_| p("    ", .{});
        std.debug.print("Type: {any}\n", .{self.ty});
    }
};
