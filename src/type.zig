const std = @import("std");

pub const Type = union(enum) {
    Function: struct {
        params: []const Type,
        ret: *const Type,
    },
    Tuple: []const Type,
    NoReturn,
    U8,
    U32,

    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .U8, .U32 => true,
            else => false,
        };
    }

    pub fn baseFmt(self: Type) []const u8 {
        return switch (self) {
            .U8 => "w",
            .U32 => "w",
            else => unreachable,
        };
    }

    pub fn extFmt(self: Type) []const u8 {
        return switch (self) {
            .U8 => "b",
            .U32 => "w",
            else => unreachable,
        };
    }

    pub fn abiFmt(self: Type) []const u8 {
        return switch (self) {
            .U8 => "ub",
            .U32 => "w",
            else => unreachable,
        };
    }

    pub fn eql(self: Type, rhs: Type) bool {
        return switch (self) {
            .Function => |l_tuple| {
                const r_tuple = if (rhs == .Function) rhs.Function else return false;
                const l_ty = Type{ .Tuple = l_tuple.params };
                const r_ty = Type{ .Tuple = r_tuple.params };
                return l_tuple.ret.eql(r_tuple.ret.*) and l_ty.eql(r_ty);
            },
            .Tuple => |a| {
                const b = if (rhs == .Tuple) rhs.Tuple else return false;
                const len = if (a.len == b.len) a.len else return false;
                for (0..len) |i|
                    if (!(a[i].coercible(b[i]))) return false;
                return true;
            },
            .U8 => rhs == .U8,
            .U32 => rhs == .U32,
            else => false,
        };
    }

    pub fn coercible(self: Type, rhs: Type) bool {
        if (self.eql(rhs)) return true;
        if (rhs == .Tuple and rhs.Tuple.len == 1 and self.coercible(rhs.Tuple[0])) return true;

        return switch (self) {
            .NoReturn => true,
            else => false,
        };
    }
};
