const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Type = lib.Type;
const Ast = lib.Ast;
const box = lib.tools.box;

pub const Context = struct {
    allocator: Allocator,
    symbols: StringHashMap(Type),
    ret: ?Type = null,

    pub fn init(allocator: Allocator) Context {
        return .{
            .allocator = allocator,
            .symbols = StringHashMap(Type).init(allocator),
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

    pub fn check(self: *Context, ast: Ast, expected: ?Type) Allocator.Error!void {
        switch (ast) {
            .Integer => {
                if (expected == null or !expected.?.isNumeric()) @panic("Can't infer type for Integer");
            },
            .Identifier => |value| {
                if (expected == null) return;
                const ty = self.symbols.get(value).?;
                if (!ty.coercible(expected.?)) @panic("Incompatible type for Identifier");
            },
            .Call => |tuple| {
                const ty = switch (tuple.f.*) {
                    .Identifier => |value| self.symbols.get(value).?,
                    else => unreachable,
                };
                if (ty != .Function) @panic("Function call on Non-Function");

                for (tuple.exprs, 0..) |expr, i|
                    try self.check(expr, ty.Function.params[i]);
            },
            .Function => |tuple| {
                var context = try self.clone();
                defer context.deinit();
                context.ret = tuple.ret;

                var params = try self.allocator.alloc(Type, tuple.params.len);
                for (tuple.params, 0..) |param, i| {
                    try context.symbols.put(param.name, param.ty);
                    params[i] = param.ty;
                }

                const ty = .{ .Function = .{
                    .params = params,
                    .ret = try box(self.allocator, tuple.ret),
                }};

                try context.symbols.put(tuple.name, ty);
                try self.symbols.put(tuple.name, ty);

                for (tuple.exprs) |expr|
                    try context.check(expr, null);
            },
            .BinOp => |tuple| {
                if (expected == null or !expected.?.isNumeric()) @panic("Can't infer type for BinOp");
                try self.check(tuple.lhs.*, expected);
                try self.check(tuple.rhs.*, expected);
            },
            .Return => |value| {
                if (self.ret == null) @panic("No return type availible");
                try self.check(value.*, self.ret.?);
            },
        }
    }
};
