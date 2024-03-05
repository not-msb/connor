const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Result(comptime T: type, comptime O: type) type {
    return struct {
        input: []const T,
        output: O,
    };
}

pub fn byte(comptime T: type, comptime source: T) fn(Allocator, []const T) Allocator.Error!?Result(T, []const T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, []const T) {
            _ = allocator;
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

pub fn bytes(comptime T: type, comptime source: []const T) fn(Allocator, []const T) Allocator.Error!?Result(T, []const T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, []const T) {
            _ = allocator;
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

pub fn tag(comptime T: type, comptime source: @typeInfo(T).Union.tag_type.?) fn(Allocator, []const T) Allocator.Error!?Result(T, T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, T) {
            _ = allocator;
            if (input.len != 0 and input[0] == source)
                return .{
                    .input = input[1..],
                    .output = input[0],
                };
            return null;
        }
    };

    return gen.f;
}

pub fn take_while1(comptime T: type, comptime func: fn(T) bool) fn(Allocator, []const T) Allocator.Error!?Result(T, []const T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, []const T) {
            const res = (try take_while0(T, func)(allocator, input)).?;
            return if (res.output.len == 0) null else res;
        }
    };

    return gen.f;
}

pub fn take_while0(comptime T: type, comptime func: fn(T) bool) fn(Allocator, []const T) Allocator.Error!?Result(T, []const T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, []const T) {
            _ = allocator;
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

pub fn many1(comptime T: type, comptime O: type, comptime func: fn(T) bool) fn(Allocator, []const T) Allocator.Error!?Result(T, []O) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, []O) {
            const res = (try many0(T, O, func)(allocator, input)).?;
            return if (res.output.len == 0) null else res;
        }
    };

    return gen.f;
}

pub fn many0(comptime T: type, comptime O: type, comptime func: anytype) fn(Allocator, []const T) Allocator.Error!?Result(T, []O) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const T) Allocator.Error!?Result(T, []O) {
            var input = _input;
            var output = try allocator.alloc(O, 0);
            while (try func(allocator, input)) |res| {
                input = res.input;
                output = try allocator.realloc(output, output.len + 1);
                output[output.len - 1] = res.output;
            }

            return .{
                .input = input,
                .output = output,
            };
        }
    };

    return gen.f;
}

pub fn manyWith0(comptime T: type, comptime O: type, comptime W: type, comptime func: anytype) fn(Allocator, []const T, W) Allocator.Error!?Result(T, []O) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const T, with: W) Allocator.Error!?Result(T, []O) {
            var input = _input;
            var output = try allocator.alloc(O, 0);
            while (try func(allocator, input, with)) |res| {
                input = res.input;
                output = try allocator.realloc(output, output.len + 1);
                output[output.len - 1] = res.output;
            }

            return .{
                .input = input,
                .output = output,
            };
        }
    };

    return gen.f;
}

pub fn alt(comptime T: type, comptime O: type, comptime funcs: anytype) fn(Allocator, []const T) Allocator.Error!?Result(T, O) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, O) {
            inline for (funcs) |func|
                if (try func(allocator, input)) |res|
                    return res;
            return null;
        }
    };

    return gen.f;
}

pub fn sequence(comptime T: type, comptime O: type, comptime funcs: anytype) fn(Allocator, []const T) Allocator.Error!?Result(T, O) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const T) Allocator.Error!?Result(T, O) {
            var input = _input;
            var output: O = undefined;

            inline for (funcs, 0..) |func, i|
                if (try func(allocator, input)) |res| {
                    input = res.input;
                    output[i] = res.output;
                } else return null;
            return .{
                .input = input,
                .output = output,
            };
        }
    };

    return gen.f;
}

pub fn sequenceWith(comptime T: type, comptime O: type, comptime W: type, comptime funcs: anytype) fn(Allocator, []const T, W) Allocator.Error!?Result(T, O) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const T, with: W) Allocator.Error!?Result(T, O) {
            var input = _input;
            var output: O = undefined;

            inline for (funcs, 0..) |func, i|
                if (try func(allocator, input, with)) |res| {
                    input = res.input;
                    output[i] = res.output;
                } else return null;
            return .{
                .input = input,
                .output = output,
            };
        }
    };

    return gen.f;
}

pub fn map(comptime T: type, comptime O: type, comptime func: anytype, comptime r_func: anytype) fn(Allocator, []const T) Allocator.Error!?Result(T, O) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, O) {
            if (try func(allocator, input)) |res| {
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

pub fn mapWith(comptime T: type, comptime O: type, comptime W: type, comptime func: anytype, comptime r_func: anytype) fn(Allocator, []const T, W) Allocator.Error!?Result(T, O) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T, with: W) Allocator.Error!?Result(T, O) {
            if (try func(allocator, input, with)) |res| {
                return .{
                    .input = res.input,
                    .output = r_func(res.output, with),
                };
            }
            return null;
        }
    };

    return gen.f;
}

pub fn mapAlloc(comptime T: type, comptime O: type, comptime func: anytype, comptime r_func: anytype) fn(Allocator, []const T) Allocator.Error!?Result(T, O) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, O) {
            if (try func(allocator, input)) |res| {
                return .{
                    .input = res.input,
                    .output = try r_func(allocator, res.output),
                };
            }
            return null;
        }
    };

    return gen.f;
}

pub fn mapAllocWith(comptime T: type, comptime O: type, comptime W: type, comptime func: anytype, comptime r_func: anytype) fn(Allocator, []const T, W) Allocator.Error!?Result(T, O) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T, with: W) Allocator.Error!?Result(T, O) {
            if (try func(allocator, input, with)) |res| {
                return .{
                    .input = res.input,
                    .output = try r_func(allocator, res.output, with),
                };
            }
            return null;
        }
    };

    return gen.f;
}

pub fn replace(comptime T: type, comptime O: type, comptime func: anytype, comptime replacement: O) fn(Allocator, []const T) Allocator.Error!?Result(T, O) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, O) {
            if (try func(allocator, input)) |res| {
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

pub fn drain(comptime T: type, comptime func: anytype) fn(Allocator, []const T) Allocator.Error!?Result(T, void) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Allocator.Error!?Result(T, void) {
            if (try func(allocator, input)) |res| {
                return .{
                    .input = res.input,
                    .output = undefined,
                };
            }
            return null;
        }
    };

    return gen.f;
}

pub fn noWith(comptime T: type, comptime O: type, comptime W: type, comptime func: anytype) fn (Allocator, []const T, W) Allocator.Error!?Result(T, O) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T, with: W) Allocator.Error!?Result(T, O) {
            _ = with;
            return func(allocator, input);
        }
    };

    return gen.f;
}
