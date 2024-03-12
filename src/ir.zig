const std = @import("std");
const lib = @import("lib.zig");
const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Type = lib.Type;
const Ast = lib.Ast;
const Context = lib.Context;
const push = lib.tools.push;
const box = lib.tools.box;

const Storage = union(enum) {
    temp: usize,
    local: []const u8,
    global: []const u8,

    fn fmt(self: Storage, allocator: Allocator) std.fmt.AllocPrintError![]const u8 {
        return switch (self) {
            .temp => |tmp| try std.fmt.allocPrint(allocator, "%t{d}", .{tmp}),
            .local => |name| try std.fmt.allocPrint(allocator, "%{s}", .{name}),
            .global => |name| try std.fmt.allocPrint(allocator, "${s}", .{name}),
        };
    }
};

pub const State = struct {
    const Symbol = enum {
        Local,
        Global,
    };

    allocator: Allocator,
    symbols: StringHashMap(Symbol),
    allocated: usize = 0,
    ret: ?Type = null,

    pub fn init(allocator: Allocator) State {
        return .{
            .allocator = allocator,
            .symbols = StringHashMap(Symbol).init(allocator),
        };
    }

    pub fn deinit(self: *State) void {
        self.symbols.deinit();
    }

    fn clone(self: *State) Allocator.Error!State {
        return .{
            .allocator = self.allocator,
            .symbols = try self.symbols.clone(),
            .allocated = self.allocated,
            .ret = self.ret,
        };
    }

    fn alloc(self: *State) Storage {
        self.allocated += 1;
        return .{ .temp = self.allocated - 1 };
    }
};

const Node = union(enum) {
    const Function_t = struct {
        name: []const u8,
        params: []const []const u8,
        blocks: []const []const Ir,
        attr: Ast.Attr,
    };

    const Call_t = struct {
        f: *const Ir,
        exprs: []const Ir,
    };

    const BinOp_t = struct {
        kind: Ast.BinOpKind,
        lhs: *const Ir,
        rhs: *const Ir,
    };

    Integer: usize,
    Identifier: []const u8,
    Call: Call_t,
    Function: Function_t,
    BinOp: BinOp_t,
    Return: *const Ir,
};

pub const Ir = struct {
    node: Node,
    ty: Type,

    pub fn from(allocator: Allocator, input: []const Ast) Allocator.Error![]Ir {
        var context = Context.init(allocator);
        defer context.deinit();

        return try fromAsts(&context, input, null);
    }

    fn fromAsts(ctx: *Context, input: []const Ast, expected: ?[]const Type) Allocator.Error![]Ir {
        var output = try ctx.allocator.alloc(Ir, input.len);
        for (input, 0..) |ast, i|
            output[i] = try Ir.fromAst(ctx, ast, if (expected) |e| e[i] else null);
        return output;
    }

    fn fromAst(ctx: *Context, input: Ast, expected: ?Type) Allocator.Error!Ir {
        return switch (input) {
            .Integer => |value| .{
                .node = .{ .Integer = value },
                .ty = expected.?,
            },
            .Identifier => |value| .{
                .node = .{ .Identifier = value },
                .ty = ctx.symbols.get(value).?,
            },
            .Call => |tuple| {
                const f = try fromAst(ctx, tuple.f.*, null);
                const ty = f.ty.Function;
                return .{
                    .node = .{ .Call = .{
                        .f = try box(ctx.allocator, f),
                        .exprs = try fromAsts(ctx, tuple.exprs, ty.params),
                    } },
                    .ty = ty.ret.*,
                };
            },
            .Function => |tuple| {
                var names = try ctx.allocator.alloc([]const u8, tuple.params.len);
                var types = try ctx.allocator.alloc(Type, tuple.params.len);
                for (tuple.params, 0..) |param, i| {
                    names[i] = param.name;
                    types[i] = param.ty;
                }

                const ty = .{ .Function = .{
                    .params = types,
                    .ret = try box(ctx.allocator, tuple.ret),
                } };

                try ctx.symbols.put(tuple.name, ty);
                var context = try ctx.clone();
                defer context.deinit();
                for (tuple.params) |param|
                    try context.symbols.put(param.name, param.ty);
                context.ret = tuple.ret;

                const exprs = try Ir.fromAsts(&context, tuple.exprs, null);

                return .{
                    .node = .{ .Function = .{
                        .name = tuple.name,
                        .params = names,
                        .blocks = try blockify(ctx.allocator, exprs),
                        .attr = tuple.attr,
                    } },
                    .ty = ty,
                };
            },
            .BinOp => |tuple| {
                const lhs = try Ir.fromAst(ctx, tuple.lhs.*, expected.?);
                const rhs = try Ir.fromAst(ctx, tuple.rhs.*, expected.?);

                return .{
                    .node = .{ .BinOp = .{
                        .kind = tuple.kind,
                        .lhs = try box(ctx.allocator, lhs),
                        .rhs = try box(ctx.allocator, rhs),
                    } },
                    .ty = lhs.ty,
                };
            },
            .Return => |value| .{
                .node = .{ .Return = try box(ctx.allocator, try Ir.fromAst(ctx, value.*, ctx.ret.?)) },
                .ty = .NoReturn,
            },
        };
    }

    fn blockify(allocator: Allocator, input: []const Ir) Allocator.Error![]const []const Ir {
        var blocks = try allocator.alloc([]const Ir, 0);
        var i: usize = 0;

        while (i < input.len) : (i += 1) {
            const start = i;
            while (i < input.len and input[i].node != .Return) : (i += 1) {}
            blocks = try push([]const Ir, allocator, blocks, input[start..i+1]);
        }

        return blocks;
    }

    pub fn compile(self: Ir, state: *State) !Storage {
        switch (self.node) {
            .Integer => |value| {
                const dst = state.alloc();
                try stdout.print("\t{s} ={s} copy {d}\n", .{ try dst.fmt(state.allocator), self.ty.baseFmt(), value });
                return dst;
            },
            .Identifier => |value| return switch (state.symbols.get(value).?) {
                .Local => .{ .local = value },
                .Global => .{ .global = value },
            },
            .Call => |tuple| {
                const params = tuple.f.ty.Function.params;
                var args = try state.allocator.alloc(Storage, tuple.exprs.len);
                for (tuple.exprs, 0..) |expr, i|
                    args[i] = try expr.compile(state);

                const dst = state.alloc();
                const src = try tuple.f.compile(state);
                try stdout.print("\t{s} ={s} call {s} (", .{
                    try dst.fmt(state.allocator),
                    self.ty.abiFmt(),
                    try src.fmt(state.allocator),
                });
                for (params, 0..) |param, i|
                    try stdout.print("{s} {s},", .{param.abiFmt(), try args[i].fmt(state.allocator)});
                try stdout.print(")\n", .{});
                return dst;
            },
            .Function => |tuple| {
                try state.symbols.put(tuple.name, .Global);

                const ty = self.ty.Function;
                var s = try state.clone();
                defer s.deinit();
                s.ret = ty.ret.*;

                for (tuple.params) |param|
                    try s.symbols.put(param, .Local);

                if (tuple.attr._export) try stdout.print("export ", .{});
                try stdout.print("function {s} ${s}(", .{ty.ret.abiFmt(), tuple.name});
                for (tuple.params, 0..) |param, i|
                    try stdout.print("{s} %{s},", .{ty.params[i].abiFmt(), param});
                try stdout.print(") {{\n", .{});
                for (tuple.blocks, 0..) |block, i| {
                    try stdout.print("@L{d}\n", .{i});
                    for (block) |expr|
                        _ = try expr.compile(&s);
                }
                try stdout.print("}}\n", .{});

                return undefined;
            },
            .BinOp => |tuple| {
                const dst = state.alloc();
                const lhs = try tuple.lhs.compile(state);
                const rhs = try tuple.rhs.compile(state);
                try stdout.print("\t{s} ={s} {s} {s}, {s}\n", .{
                    try dst.fmt(state.allocator),
                    self.ty.baseFmt(),
                    tuple.kind.fmt(),
                    try lhs.fmt(state.allocator),
                    try rhs.fmt(state.allocator),
                });
                return dst;
            },
            .Return => |value| {
                const src = try value.compile(state);
                try stdout.print("\tret {s}\n", .{try src.fmt(state.allocator)});

                return undefined;
            },
        }
    }
};
