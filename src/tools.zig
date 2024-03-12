const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn box(allocator: Allocator, value: anytype) Allocator.Error!*@TypeOf(value) {
    const boxed = try allocator.create(@TypeOf(value));
    boxed.* = value;
    return boxed;
}

pub fn append(comptime T: type, allocator: Allocator, slice: []T, value: []const T) Allocator.Error![]T {
    var new = try allocator.realloc(slice, slice.len + value.len);
    @memcpy(new[new.len-value.len..], value);
    return new;
}

pub fn push(comptime T: type, allocator: Allocator, slice: []T, value: T) Allocator.Error![]T {
    var new = try allocator.realloc(slice, slice.len + 1);
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

pub fn unTag(comptime I: type, comptime O: type, comptime source: @typeInfo(I).Union.tag_type.?) fn (I) O {
    const gen = struct {
        fn f(input: I) O {
            return @field(input, @tagName(source));
        }
    };

    return gen.f;
}

pub fn getError(comptime T: type) type {
    const G = switch (@typeInfo(T)) {
        .Pointer => |t| t.child,
        else => T,
    };
    return switch (@typeInfo(G)) {
        .Struct, .Enum, .Union, .Opaque => if (@hasDecl(G, "Error")) G.Error else error{},
        else => error{},
    };
}
