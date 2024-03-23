const std = @import("std");

pub const Type = union(enum) {
    Function: struct {
        params: []const Type,
        ret: *const Type,
    },
    Void,
    NoReturn,
    U8,
    U32,
    U64,

    pub fn size(self: Type) ?usize {
        return switch (self) {
            .Void => 0,
            .U8 => 1,
            .U32 => 4,
            .U64 => 8,
            else => null,
        };
    }

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

    pub fn min(self: Type, rhs: Type) Type {
        return if (self.size().? >= rhs.size().?) rhs else self;
    }

    pub fn eql(self: Type, rhs: Type) bool {
        return switch (self) {
            .Void => rhs == .Void,
            .NoReturn => rhs == .NoReturn,
            .U8 => rhs == .U8,
            .U32 => rhs == .U32,
            .U64 => rhs == .U64,
            else => false,
        };
    }

    pub fn coercible(self: Type, rhs: Type) bool {
        if (self.eql(rhs)) return true;
        if (self.isNumeric() and rhs.isNumeric() and self.size().? >= rhs.size().?) return true;

        return switch (self) {
            .NoReturn => true,
            else => false,
        };
    }
};
