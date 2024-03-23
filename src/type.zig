const std = @import("std");

pub const Type = union(enum) {
    Function: struct {
        params: []const Type,
        ret: *const Type,
    },
    Void,
    NoReturn,
    CompInt,
    U8,
    U32,
    U64,

    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .U8, .U32, .U64 => true,
            else => false,
        };
    }

    pub fn baseFmt(self: Type) []const u8 {
        return switch (self) {
            .U8 => "w",
            .U32 => "w",
            .U64 => "l",
            else => unreachable,
        };
    }

    pub fn extFmt(self: Type) []const u8 {
        return switch (self) {
            .U8 => "b",
            .U32 => "w",
            .U64 => "l",
            else => unreachable,
        };
    }

    pub fn abiFmt(self: Type) []const u8 {
        return switch (self) {
            .NoReturn => "w",
            .U8 => "ub",
            .U32 => "w",
            .U64 => "l",
            else => unreachable,
        };
    }

    pub fn eql(self: Type, rhs: Type) bool {
        return switch (self) {
            .CompInt => rhs == .CompInt,
            .U8 => rhs == .U8,
            .U32 => rhs == .U32,
            .U64 => rhs == .U64,
            else => false,
        };
    }

    pub fn coercible(self: Type, rhs: Type) bool {
        if (self.eql(rhs)) return true;

        return switch (self) {
            .NoReturn => true,
            .CompInt => rhs.isNumeric(),
            else => false,
        };
    }
};
