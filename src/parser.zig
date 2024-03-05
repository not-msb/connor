// I have pity for they who try to understand anything written down below!

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

pub fn manyWith0(comptime W: type, comptime func: anytype) replaceOutputFullWith([]outputType(func), W, func) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const inputType(func), with: W) replaceOutput([]outputType(func), func) {
            var input = _input;
            var output = try allocator.alloc(outputType(func), 0);
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

pub fn alt(comptime funcs: anytype) @TypeOf(funcs[0]) {
    const first = funcs[0];
    const gen = struct {
        fn f(allocator: Allocator, input: []const inputType(first)) returnType(first) {
            inline for (funcs) |func|
                if (try func(allocator, input)) |res|
                    return res;
            return null;
        }
    };

    return gen.f;
}

pub fn sequence(comptime funcs: anytype) replaceOutputFull(outputTuple(funcs), funcs[0]) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const inputType(funcs[0])) replaceOutput(outputTuple(funcs), funcs[0]) {
            var input = _input;
            var output: outputTuple(funcs) = undefined;

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

pub fn sequenceWith(comptime W: type, comptime funcs: anytype) replaceOutputFullWith(outputTuple(funcs), W, funcs[0]) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const inputType(funcs[0]), with: W) replaceOutput(outputTuple(funcs), funcs[0]) {
            var input = _input;
            var output: outputTuple(funcs) = undefined;

            inline for (funcs, 0..) |func, i| {
                const r = if (getWithType(func)) |_| func(allocator, input, with) else func(allocator, input);
                if (try r) |res| {
                    input = res.input;
                    output[i] = res.output;
                } else return null;
            }

            return .{
                .input = input,
                .output = output,
            };
        }
    };

    return gen.f;
}

pub fn map(comptime W: ?type, comptime func: anytype, comptime r_func: anytype) if (W) |w| replaceOutputFullWith(returnTypeErr(r_func), w, func) else replaceOutputFull(returnTypeErr(r_func), func) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const inputType(func)) replaceOutput(returnTypeErr(r_func), func) {
            const allocates = comptime allocsError(r_func);

            if (try func(allocator, input)) |res| {
                const output = if (allocates)
                    try r_func(allocator, res.output) else
                    r_func(res.output);
                return .{
                    .input = res.input,
                    .output = output,
                };
            }
            return null;
        }

        fn g(allocator: Allocator, input: []const inputType(func), with: W.?) replaceOutput(returnTypeErr(r_func), func) {
            const f_args = if (getWithType(func)) |_|
                .{ allocator, input, with } else
                .{ allocator, input };

            if (try @call(.auto, func, f_args)) |res| {
                const allocates = comptime allocsError(r_func);
                const uses_with = comptime getWithType(r_func) != null;
                const output =
                    if (!allocates and !uses_with) r_func(res.output) else
                    if (!allocates and  uses_with) r_func(res.output, with) else
                    if ( allocates and !uses_with) try r_func(allocator, res.output) else
                    if ( allocates and  uses_with) try r_func(allocator, res.output, with);
                return .{
                    .input = res.input,
                    .output = output,
                };
            }
            return null;
        }
    };

    return if (W) |_| gen.g else gen.f;
}

pub fn replace(comptime func: anytype, comptime repl: anytype) replaceOutputFull(@TypeOf(repl), func) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const inputType(func)) replaceOutput(@TypeOf(repl), func) {
            if (try func(allocator, input)) |res| {
                return .{
                    .input = res.input,
                    .output = repl,
                };
            }
            return null;
        }
    };

    return gen.f;
}

pub fn drain(comptime func: anytype) replaceOutputFull(void, func) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const inputType(func)) replaceOutput(void, func) {
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

// Utilities
fn returnType(func: anytype) type {
    return @typeInfo(@TypeOf(func)).Fn.return_type.?;
}

fn returnTypeErr(func: anytype) type {
    const T = returnType(func);
    return if (@typeInfo(T) == .ErrorUnion)
        @typeInfo(T).ErrorUnion.payload else
        T;
}

fn inputType(func: anytype) type {
    const params = @typeInfo(@TypeOf(func)).Fn.params;
    return if (params[0].type.? == Allocator)
        @typeInfo(params[1].type.?).Pointer.child else
        @typeInfo(params[0].type.?).Pointer.child;
}

fn getWithType(func: anytype) ?type {
    const params = @typeInfo(@TypeOf(func)).Fn.params;
    if (params.len <= 1) return null;
    if (params[0].type != null and params[0].type.? == Allocator)
        return if (params.len == 3) params[2].type else null;
    return params[params.len-1].type;
}

fn outputType(func: anytype) type {
    return @typeInfo(@typeInfo(@typeInfo(@typeInfo(@TypeOf(func)).Fn.return_type.?).ErrorUnion.payload).Optional.child).Struct.fields[1].type;
}

fn allocsError(func: anytype) bool {
    const params = @typeInfo(@TypeOf(func)).Fn.params;
    const ret = @typeInfo(@TypeOf(func)).Fn.return_type.?;
    return (@typeInfo(ret) == .ErrorUnion or @typeInfo(ret) == .ErrorSet) and (params.len != 0 and params[0].type == Allocator);
}

fn outputTuple(funcs: anytype) type {
    var types: [funcs.len]type = undefined;
    for (funcs, 0..) |func, i|
        types[i] = outputType(func);
    return std.meta.Tuple(&types);
}

fn replaceOutputFull(comptime O: type, func: anytype) type {
    return fn(Allocator, []const inputType(func)) Allocator.Error!?Result(inputType(func), O);
}

fn replaceOutputFullWith(comptime O: type, comptime W: type, func: anytype) type {
    return fn(Allocator, []const inputType(func), W) Allocator.Error!?Result(inputType(func), O);
}

fn replaceOutput(comptime O: type, func: anytype) type {
    return Allocator.Error!?Result(inputType(func), O);
}
