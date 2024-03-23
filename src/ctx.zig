const std = @import("std");
const lib = @import("lib.zig");
const panic = std.debug.panic;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Type = lib.Type;
const File = lib.File;
const Ast = lib.Ast;
const box = lib.tools.box;

pub const Symbol = struct {
    const Scope = enum {
        local,
        global,

        pub fn fmt(self: Scope) u8 {
            return switch (self) {
                .local => '%',
                .global => '$',
            };
        }
    };

    scope: Scope,
    ty: Type,
};

pub const Context = struct {
    allocator: Allocator,
    symbols: StringHashMap(Symbol),
    ret: ?Type = null,

    pub fn init(allocator: Allocator) Context {
        return .{
            .allocator = allocator,
            .symbols = StringHashMap(Symbol).init(allocator),
        };
    }

    pub fn deinit(self: *Context) void {
        self.symbols.deinit();
    }

    pub fn clone(self: *const Context) Allocator.Error!Context {
        return .{
            .allocator = self.allocator,
            .symbols = try self.symbols.clone(),
        };
    }

    pub fn scan(allocator: Allocator, file: File) Allocator.Error!void {
        var ctx = Context.init(allocator);
        var symbols = file.symbols.iterator();
        var functions = file.functions.iterator();

        while (symbols.next()) |entry|
            try ctx.symbols.put(entry.key_ptr.*, .{
                .scope = .global,
                .ty = entry.value_ptr.*,
            });

        while (functions.next()) |entry| {
            const func = entry.value_ptr.*;
            const f = func.ty.Function;
            try ctx.symbols.put(entry.key_ptr.*, .{ .scope = .global, .ty = func.ty });

            var context = try ctx.clone();
            context.ret = f.ret.*;
            for (func.params, 0..) |name, i|
                try context.symbols.put(name, .{ .scope = .local, .ty = f.params[i] });

            const ret = try context.check(func.expr);
            if (!ret.coercible(f.ret.*)) panic("Incompatible return type", .{});
        }
    }

    fn check(self: *Context, ast: Ast) Allocator.Error!Type {
        switch (ast) {
            .Integer => return .U64,
            .Identifier => |v| return self.symbols.get(v).?.ty,
            .Block => |v| {
                var ret: ?Type = null;
                for (v) |expr| {
                    const ty = try self.check(expr);
                    if (ty == .NoReturn) ret = .NoReturn;
                }
                return ret orelse .Void;
            },
            .Call => |t| {
                const f = try self.check(t.f.*);
                if (f != .Function) panic("Function call on Non-Function", .{});
                for (t.exprs, 0..) |expr, i| {
                    const e = try self.check(expr);
                    if (!e.coercible(f.Function.params[i])) panic("Incompatible parameter type", .{});
                }
                return f.Function.ret.*;
            },
            .BinOp => |t| {
                const lhs = try self.check(t.lhs.*);
                const rhs = try self.check(t.rhs.*);
                if (!(lhs.isNumeric() or rhs.isNumeric())) panic("BinOp on non-number", .{});
                return lhs.min(rhs);
            },
            .Return => |v| {
                const ty = try self.check(v.*);
                if (!ty.coercible(self.ret.?)) panic("Incompatible return type", .{});
                return .NoReturn;
            },
        }
    }
};
