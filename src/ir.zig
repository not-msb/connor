const std = @import("std");
const lib = @import("lib.zig");
const stdout = std.io.getStdOut().writer();
const panic = std.debug.panic;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const StringArrayHashMap = std.StringArrayHashMap;
const Type = lib.Type;
const File = lib.File;
const Ast = lib.Ast;
const Context = lib.Context;
const box = lib.tools.box;

pub const Storage = union(enum) {
    literal: usize,
    temp: struct { usize, Type }, // local
    named: []const u8,

    fn ty(self: Storage, ctx: *const Context) Type {
        return switch (self) {
            .literal => .U64,
            .temp => |t| t[1],
            .named => |v| ctx.symbols.get(v).?.ty,
        };
    }

    fn fmt(self: Storage, ctx: *const Context) std.fmt.AllocPrintError![]const u8 {
        return switch (self) {
            .literal => |v| try std.fmt.allocPrint(ctx.allocator, "{d}", .{v}),
            .temp => |t| try std.fmt.allocPrint(ctx.allocator, "%t{d}", .{t[0]}),
            .named => |v| try std.fmt.allocPrint(ctx.allocator, "{c}{s}", .{ ctx.symbols.get(v).?.scope.fmt(), v }),
        };
    }
};

pub const IrFile = struct {
    const Function = struct {
        params: [][]const u8,
        attr: Ast.Attr,
        block: []const Ir,
        ty: Type,
    };

    ctx: Context,
    functions: StringArrayHashMap(Function),

    pub fn from(allocator: Allocator, file: File) Allocator.Error!IrFile {
        var ctx = Context.init(allocator);
        var symbols = file.symbols.iterator();
        var functions = file.functions.iterator();
        var list = StringArrayHashMap(IrFile.Function).init(ctx.allocator);

        while (symbols.next()) |entry|
            try ctx.symbols.put(entry.key_ptr.*, .{ .scope = .global, .ty = entry.value_ptr.* });

        while (functions.next()) |entry| {
            const name = entry.key_ptr.*;
            const func = entry.value_ptr.*;
            const f = func.ty.Function;
            try ctx.symbols.put(name, .{
                .scope = .global,
                .ty = func.ty,
            });

            var context = try ctx.clone();
            context.ret = f.ret.*;
            for (func.params, 0..) |param, i|
                try context.symbols.put(param, .{
                    .scope = .local,
                    .ty = f.params[i],
                });

            const block = switch (func.expr) {
                .Block => try Ir.from(&context, func.expr),
                else => try Ir.from(&context, .{ .Return = try box(allocator, func.expr) }),
            };

            try list.put(name, .{
                .params = func.params,
                .attr = func.attr,
                .block = block,
                .ty = func.ty,
            });
        }

        return .{
            .ctx = ctx,
            .functions = list,
        };
    }
    pub fn compile(self: IrFile) !void {
        var functions = self.functions.iterator();
        while (functions.next()) |entry| {
            const name = entry.key_ptr.*;
            const func = entry.value_ptr.*;
            const f = func.ty.Function;

            var context = try self.ctx.clone();
            context.ret = f.ret.*;
            for (func.params, f.params) |param, ty|
                try context.symbols.put(param, .{
                    .scope = .local,
                    .ty = ty,
                });

            if (func.attr._export)
                try stdout.writeAll("export ");
            try stdout.print("function {s} ${s}(", .{ f.ret.abiFmt(), name });
            for (func.params, f.params) |param, ty|
                try stdout.print("{s} %{s},", .{ ty.abiFmt(), param });
            try stdout.writeAll(") {\n@L0\n");
            var label: usize = 0;
            for (func.block, 0..) |ir, i| {
                try ir.compile(&context);
                if (ir == .Return and i != func.block.len - 1) {
                    label += 1;
                    try stdout.print("@L{d}\n", .{label});
                }
            }
            try stdout.writeAll("}\n");
        }
    }
};

pub const Ir = union(enum) {
    const Call_t = struct {
        f: Storage,
        dst: Storage,
        src: []const Storage,
    };

    const BinOp_t = struct {
        kind: Ast.BinOpKind,
        dst: Storage,
        lhs: Storage,
        rhs: Storage,
    };

    Nop: Storage,
    Call: Call_t,
    BinOp: BinOp_t,
    Return: Storage,

    fn from(ctx: *Context, input: Ast) Allocator.Error![]Ir {
        var list = ArrayList(Ir).init(ctx.allocator);
        switch (input) {
            .Integer => |v| try list.append(.{ .Nop = .{ .literal = v } }),
            .Identifier => |v| try list.append(.{ .Nop = .{ .named = v } }),
            .Block => |v| {
                for (v) |item| {
                    const slice = try from(ctx, item);
                    try list.appendSlice(slice);
                }
            },
            .Call => |t| {
                const f = try from(ctx, t.f.*);
                const ret = f[f.len - 1].dest().ty(ctx).Function.ret.*;

                var src = ArrayList(Storage).init(ctx.allocator);
                for (t.exprs) |expr| {
                    const e = try from(ctx, expr);
                    try src.append(e[e.len - 1].dest());
                    try list.appendSlice(e);
                }

                try list.append(.{ .Call = .{
                    .f = f[f.len - 1].dest(),
                    .dst = nextStorage(list.items, ret),
                    .src = try src.toOwnedSlice(),
                } });
            },
            .BinOp => |t| {
                const lhs = try from(ctx, t.lhs.*);
                const rhs = try from(ctx, t.rhs.*);
                const ty = lhs[lhs.len - 1].dest().ty(ctx); // TODO: Improve Type inference
                try list.appendSlice(lhs);
                try list.appendSlice(rhs);
                try list.append(.{ .BinOp = .{
                    .kind = t.kind,
                    .dst = nextStorage(list.items, ty),
                    .lhs = lhs[lhs.len - 1].dest(),
                    .rhs = rhs[rhs.len - 1].dest(),
                } });
            },
            .Return => |v| {
                const ret = try from(ctx, v.*);
                try list.appendSlice(ret);
                try list.append(.{ .Return = ret[ret.len - 1].dest() });
            },
        }
        return list.toOwnedSlice();
    }

    fn compile(self: Ir, ctx: *const Context) !void {
        switch (self) {
            .Nop => {},
            .Call => |t| {
                try stdout.print("\t{s} ={s} call {s}(", .{
                    try t.dst.fmt(ctx),
                    t.dst.ty(ctx).abiFmt(),
                    try t.f.fmt(ctx),
                });
                for (t.src) |ir|
                    try stdout.print("{s} {s},", .{ ir.ty(ctx).abiFmt(), try ir.fmt(ctx) });
                try stdout.writeAll(")\n");
            },
            .BinOp => |t| {
                try stdout.print("\t{s} ={s} {s} {s}, {s}\n", .{
                    try t.dst.fmt(ctx),
                    t.dst.ty(ctx).abiFmt(),
                    t.kind.fmt(),
                    try t.lhs.fmt(ctx),
                    try t.rhs.fmt(ctx),
                });
            },
            .Return => |v| {
                try stdout.print("\tret {s}\n", .{try v.fmt(ctx)});
            },
        }
    }

    fn dest(self: Ir) Storage {
        return switch (self) {
            .Nop => |v| v,
            .BinOp => |t| t.dst,
            .Call => |t| t.dst,
            .Return => unreachable, // TODO: Handle
        };
    }

    fn nextStorage(self: []const Ir, ty: Type) Storage {
        var temp: usize = 0;
        for (self) |item| {
            // This assumes the ir allocates in order
            if (item.dest() == .temp and item.dest().temp[0] == temp) temp += 1;
        }
        return .{ .temp = .{ temp, ty } };
    }
};
