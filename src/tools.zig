const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn box(allocator: Allocator, value: anytype) Allocator.Error!*@TypeOf(value) {
    const boxed = try allocator.create(@TypeOf(value));
    boxed.* = value;
    return boxed;
}

pub fn push(comptime T: type, allocator: Allocator, slice: []T, value: T) Allocator.Error![]T {
    var new = try allocator.realloc(slice, slice.len + 1);
    new[slice.len] = value;
    return new;
}

pub fn index(comptime T: type, comptime S: type, comptime idx: usize) fn (S, slice: anytype) getError(S)!T {
    const gen = struct {
        fn f(state: S, slice: anytype) getError(S)!T {
            _ = state;
            return slice[idx];
        }
    };

    return gen.f;
}

pub fn unTag(comptime I: type, comptime O: type, comptime S: type, comptime source: @typeInfo(I).Union.tag_type.?) fn (S, I) getError(S)!O {
    const gen = struct {
        fn f(state: S, input: I) getError(S)!O {
            _ = state;
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
