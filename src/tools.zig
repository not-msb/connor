const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn box(allocator: Allocator, value: anytype) Allocator.Error!*@TypeOf(value) {
    const boxed = try allocator.create(@TypeOf(value));
    boxed.* = value;
    return boxed;
}
