const std = @import("std");

pub fn Result(comptime T: type, comptime O: type) type {
    return struct {
        input: []const T,
        output: O,
    };
}

pub fn byte(comptime T: type, comptime source: T) fn([]const T) ?Result(T, []const u8) {
    const gen = struct {
        fn f(input: []const T) ?Result(T, []const u8) {
            if (input.len != 0 and input[0] == source)
                return .{
                    .input = input[1..],
                    .output = input[0..1],
                };
            return null;
        }
    };

    return gen.f;
}

pub fn tag(comptime T: type, comptime source: []const T) fn([]const T) ?Result(T, []const u8) {
    const gen = struct {
        fn f(input: []const T) ?Result(T, []const u8) {
            if (std.mem.startsWith(T, input, source))
                return .{
                    .input = input[source.len..],
                    .output = input[0..source.len],
                };
            return null;
        }
    };

    return gen.f;
}

pub fn take_while1(comptime T: type, comptime func: fn(T) bool) fn([]const T) ?Result(T, []const T) {
    const gen = struct {
        fn f(input: []const T) ?Result(T, []const T) {
            const res = take_while0(T, func)(input);
            return if (res.output.len == 0) null else res;
        }
    };

    return gen.f;
}

pub fn take_while0(comptime T: type, comptime func: fn(T) bool) fn([]const T) Result(T, []const T) {
    const gen = struct {
        fn f(input: []const T) Result(T, []const T) {
            var i: usize = 0;
            while (i < input.len and func(input[i])) : (i += 1) {}
            return .{
                .input = input[i..],
                .output = input[0..i],
            };
        }
    };

    return gen.f;
}

pub fn alt(comptime T: type, comptime O: type, comptime funcs: anytype) fn([]const T) ?Result(T, O) {
    const gen = struct {
        fn f(input: []const T) ?Result(T, O) {
            inline for (funcs) |func| {
                if (func(input)) |res| {
                    return res;
                }
            }
            return null;
        }
    };

    return gen.f;
}

pub fn map(comptime T: type, comptime O: type, comptime func: anytype, comptime r_func: anytype) fn([]const T) ?Result(T, O) {
    const gen = struct {
        fn f(input: []const T) ?Result(T, O) {
            if (func(input)) |res| {
                return .{
                    .input = res.input,
                    .output = r_func(res.output),
                };
            }
            return null;
        }
    };

    return gen.f;
}

pub fn replace(comptime T: type, comptime O: type, comptime func: anytype, comptime replacement: O) fn([]const T) ?Result(T, O) {
    const gen = struct {
        fn f(input: []const T) ?Result(T, O) {
            if (func(input)) |res| {
                return .{
                    .input = res.input,
                    .output = replacement,
                };
            }
            return null;
        }
    };

    return gen.f;
}
