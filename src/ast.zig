const std = @import("std");
const lib = @import("lib.zig");
const panic = std.debug.panic;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const StringArrayHashMap = std.StringArrayHashMap;
const Context = lib.Context;
const Lexer = lib.Lexer;
const Token = lib.Token;
const Type = lib.Type;
const box = lib.tools.box;

pub const File = struct {
    const Function = struct {
        params: [][]const u8,
        attr: Ast.Attr,
        expr: Ast,
        ty: Type,
    };

    symbols: StringArrayHashMap(Type),
    functions: StringArrayHashMap(Function),

    // TODO: Move preprocessor here
    pub fn parse(allocator: Allocator, input: []const u8) Allocator.Error!File {
        var lexer = try Lexer.parse(allocator, input);
        defer lexer.deinit();
        var symbols = StringArrayHashMap(Type).init(allocator);
        var functions = StringArrayHashMap(Function).init(allocator);

        while (lexer.tokens.items.len != 0) {
            if (try Ast._extern(&lexer)) |t| {
                try symbols.put(t.name, t.ty);
            } else if (try Ast._function(&lexer)) |t| {
                try functions.put(t.name, t.func);
            } else @panic("Couldn't parse");
        }

        return .{
            .symbols = symbols,
            .functions = functions,
        };
    }
};

pub const Ast = union(enum) {
    pub const Param = struct {
        name: []const u8,
        ty: Type,
    };

    pub const Attr = struct {
        _export: bool = false,
    };

    const Call_t = struct {
        f: *const Ast,
        exprs: []const Ast,
    };

    pub const BinOpKind = enum {
        Add,

        pub fn fmt(self: BinOpKind) []const u8 {
            return switch (self) {
                .Add => "add",
            };
        }
    };

    const BinOp_t = struct {
        kind: BinOpKind,
        lhs: *const Ast,
        rhs: *const Ast,
    };

    Integer: usize,
    Identifier: []const u8,
    Block: []const Ast,
    Call: Call_t,
    BinOp: BinOp_t,
    Return: *const Ast,

    fn _extern_params(_lexer: *Lexer) Allocator.Error!?[]Type {
        var lexer = try _lexer.clone();
        var params = ArrayList(Type).init(_lexer.allocator);

        b: {
            _ = if (lexer.peek() == .LParen) lexer.next() else break :b;
            while (true) {
                if (lexer.peek() == .RParen) break;
                const param = if (lexer.peek() == .Type) lexer.next().Type else break :b;
                try params.append(param);
                if (lexer.peek() == .Comma) _ = lexer.next() else break;
            }
            _ = if (lexer.peek() == .RParen) lexer.next() else break :b;

            _lexer.deinit();
            _lexer.* = lexer;
            return try params.toOwnedSlice();
        }

        lexer.deinit();
        return null;
    }

    fn _params(_lexer: *Lexer) Allocator.Error!?struct { names: [][]const u8, types: []Type } {
        var lexer = try _lexer.clone();
        var names = ArrayList([]const u8).init(_lexer.allocator);
        var types = ArrayList(Type).init(_lexer.allocator);

        b: {
            _ = if (lexer.peek() == .LParen) lexer.next() else break :b;
            while (true) {
                if (lexer.peek() == .RParen) break;
                const ty = if (lexer.peek() == .Type) lexer.next().Type else break :b;
                const name = if (lexer.peek() == .Word) lexer.next().Word else break :b;
                try names.append(name);
                try types.append(ty);
                if (lexer.peek() == .Comma) _ = lexer.next() else break;
            }
            _ = if (lexer.peek() == .RParen) lexer.next() else break :b;

            _lexer.deinit();
            _lexer.* = lexer;
            return .{
                .names = try names.toOwnedSlice(),
                .types = try types.toOwnedSlice(),
            };
        }

        lexer.deinit();
        return null;
    }

    fn _extern(_lexer: *Lexer) Allocator.Error!?struct { name: []const u8, ty: Type } {
        var lexer = try _lexer.clone();

        b: {
            _ = if (lexer.peek() == .Extern) lexer.next() else break :b;
            const ext = .{
                .ret = if (lexer.peek() == .Type) lexer.next().Type else break :b,
                .name = if (lexer.peek() == .Word) lexer.next().Word else break :b,
                .params = try _extern_params(&lexer) orelse break :b,
            };
            _ = if (lexer.peek() == .SemiColon) lexer.next() else break :b;

            _lexer.deinit();
            _lexer.* = lexer;
            return .{
                .name = ext.name,
                .ty = .{ .Function = .{
                    .params = ext.params,
                    .ret = try box(lexer.allocator, ext.ret),
                } },
            };
        }

        lexer.deinit();
        return null;
    }

    fn _function(_lexer: *Lexer) Allocator.Error!?struct { name: []const u8, func: File.Function } {
        var lexer = try _lexer.clone();

        b: {
            const _export: ?void = if (lexer.peek() == .Export) lexer.next().Export else null;
            const func = .{
                .ret = if (lexer.peek() == .Type) lexer.next().Type else break :b,
                .name = if (lexer.peek() == .Word) lexer.next().Word else break :b,
                .params = try _params(&lexer) orelse break :b,
                .expr = try expr(&lexer, 0),
                .attr = .{
                    ._export = _export != null,
                },
            };
            _ = if (lexer.peek() == .SemiColon) lexer.next() else break :b;

            _lexer.deinit();
            _lexer.* = lexer;
            return .{
                .name = func.name,
                .func = .{
                    .params = func.params.names,
                    .attr = func.attr,
                    .expr = func.expr,
                    .ty = .{ .Function = .{
                        .params = func.params.types,
                        .ret = try box(lexer.allocator, func.ret),
                    } },
                },
            };
        }

        lexer.deinit();
        return null;
    }

    // Pratt Parser
    pub fn expr(lexer: *Lexer, min_power: u8) Allocator.Error!Ast {
        var lhs: Ast = switch (lexer.next()) {
            .Eof => panic("Reached Eof", .{}),
            .LBracket => b: {
                var exprs = ArrayList(Ast).init(lexer.allocator);
                while (true) {
                    if (lexer.peek() == .RBracket) break;
                    const e = try expr(lexer, 0);
                    try exprs.append(e);
                    std.debug.assert(lexer.next() == .SemiColon);
                }
                std.debug.assert(lexer.next() == .RBracket);
                break :b .{ .Block = try exprs.toOwnedSlice() };
            },
            .Return => b: {
                const lhs = try expr(lexer, prefixPower(.Return));
                break :b .{ .Return = try box(lexer.allocator, lhs) };
            },
            .Integer => |v| .{ .Integer = v },
            .Word => |v| .{ .Identifier = v },
            else => |token| panic("Unexpected Token: {}", .{token}),
        };

        while (true) {
            const token = lexer.peek();
            const op = switch (token) {
                .Eof, .SemiColon, .Comma, .RParen => break,
                .Add, .LParen => token,
                else => panic("Unexpected Token: {}", .{token}),
            };

            if (postfixPower(op)) |power| {
                if (power < min_power) break;
                _ = lexer.next();

                switch (op) {
                    .LParen => {
                        var exprs = ArrayList(Ast).init(lexer.allocator);

                        while (true) {
                            if (lexer.peek() == .RParen) break;
                            const e = try expr(lexer, 0);
                            try exprs.append(e);
                            if (lexer.peek() == .Comma) _ = lexer.next() else break;
                        }

                        std.debug.assert(lexer.next() == .RParen);
                        lhs = .{ .Call = .{
                            .f = try box(lexer.allocator, lhs),
                            .exprs = try exprs.toOwnedSlice(),
                        } };
                    },
                    else => unreachable,
                }

                continue;
            }

            if (infixPower(op)) |power| {
                if (power.l < min_power) break;
                _ = lexer.next();

                const rhs = try expr(lexer, power.r);
                const kind: Ast.BinOpKind = switch (op) {
                    .Add => .Add,
                    else => unreachable,
                };

                lhs = .{ .BinOp = .{
                    .kind = kind,
                    .lhs = try box(lexer.allocator, lhs),
                    .rhs = try box(lexer.allocator, rhs),
                } };
                continue;
            }

            break;
        }

        return lhs;
    }

    fn prefixPower(op: Token) u8 {
        return switch (op) {
            .Return => 2,
            else => panic("Unexpected Token: {}", .{op}),
        };
    }

    fn postfixPower(op: Token) ?u8 {
        return switch (op) {
            .LParen => 10,
            else => null,
        };
    }

    fn infixPower(op: Token) ?struct { l: u8, r: u8 } {
        return switch (op) {
            .Add => .{ .l = 4, .r = 5 },
            else => null,
        };
    }
};
