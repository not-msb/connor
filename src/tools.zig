const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn box(allocator: Allocator, value: anytype) Allocator.Error!*@TypeOf(value) {
    const boxed = try allocator.create(@TypeOf(value));
    boxed.* = value;
    return boxed;
}

pub fn push(comptime T: type, allocator: Allocator, slice: []T, value: T) Allocator.Error![]T {
    var new = try allocator.realloc(slice, slice.len+1);
    new[slice.len] = value;
    return new;
}

pub fn index(comptime T: type, comptime idx: usize) fn (slice: anytype) T {
    const gen = struct {
        fn f(slice: anytype) T {
            return slice[idx];
        }
    };

    return gen.f;
}

pub fn indexWith(comptime T: type, comptime W: type, comptime idx: usize) fn (slice: anytype, W) T {
    const gen = struct {
        fn f(slice: anytype, with: W) T {
            _ = with;
            return slice[idx];
        }
    };

    return gen.f;
}

pub fn unTag(comptime T: type, comptime O: type, comptime source: @typeInfo(T).Union.tag_type.?) fn(T) O {
    const gen = struct {
        fn f(input: T) O {
            return @field(input, @tagName(source));
        }
    };

    return gen.f;
}
