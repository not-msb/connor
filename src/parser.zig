const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const tools = lib.tools;

pub fn Parser(comptime I: type, comptime O: type) type {
    return struct {
        _parse: fn (Allocator, []const I) Result,

        const Self = @This();
        pub const Result = Allocator.Error!?struct {
            input: []const I,
            output: O,
        };

        pub fn in(self: Self) type {
            _ = self;
            return I;
        }

        pub fn out(self: Self) type {
            _ = self;
            return O;
        }

        pub fn parse(self: Self, allocator: Allocator, input: []const I) Result {
            return self._parse(allocator, input);
        }
    };
}

pub fn byte(comptime T: type, comptime source: T) Parser(T, []const T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Parser(T, []const T).Result {
            _ = allocator;
            if (input.len != 0 and input[0] == source)
                return .{
                    .input = input[1..],
                    .output = input[0..1],
                };
            return null;
        }
    };

    return .{ ._parse = gen.f };
}

pub fn bytes(comptime T: type, comptime source: []const T) Parser(T, []const T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Parser(T, []const T).Result {
            _ = allocator;
            if (std.mem.startsWith(u8, input, source))
                return .{
                    .input = input[source.len..],
                    .output = input[0..source.len],
                };
            return null;
        }
    };

    return .{ ._parse = gen.f };
}

pub fn tag(comptime T: type, comptime source: @typeInfo(T).Union.tag_type.?) Parser(T, T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Parser(T, T).Result {
            _ = allocator;
            if (input.len != 0 and input[0] == source)
                return .{
                    .input = input[1..],
                    .output = input[0],
                };
            return null;
        }
    };

    return .{ ._parse = gen.f };
}

pub fn map(comptime parser: anytype, comptime r_func: anytype) Parser(parser.in(), stripError(returnType(r_func))) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const parser.in()) Parser(parser.in(), stripError(returnType(r_func))).Result {
            const result = try parser.parse(allocator, input) orelse return null;
            return .{
                .input = result.input,
                .output = r_func(result.output),
            };
        }
    };

    return .{ ._parse = gen.f };
}

pub fn mapWith(comptime parser: anytype, comptime r_func: anytype) Parser(parser.in(), stripError(returnType(r_func))) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const parser.in()) Parser(parser.in(), stripError(returnType(r_func))).Result {
            const result = try parser.parse(allocator, input) orelse return null;
            return .{
                .input = result.input,
                .output = try r_func(allocator, result.output),
            };
        }
    };

    return .{ ._parse = gen.f };
}

fn returnType(comptime func: anytype) type {
    return @typeInfo(@TypeOf(func)).Fn.return_type.?;
}

fn stripError(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .ErrorUnion => |t| t.payload,
        else => T,
    };
}

pub fn replace(comptime parser: anytype, comptime repl: anytype) Parser(parser.in(), @TypeOf(repl)) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const parser.in()) Parser(parser.in(), @TypeOf(repl)).Result {
            const result = try parser.parse(allocator, input) orelse return null;
            return .{
                .input = result.input,
                .output = repl,
            };
        }
    };

    return .{ ._parse = gen.f };
}

// TODO: Add clone checking
pub fn alt(comptime parsers: anytype) @TypeOf(parsers[0]) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const parsers[0].in()) @TypeOf(parsers[0]).Result {
            inline for (parsers) |parser|
                if (try parser.parse(allocator, input)) |result|
                    return result;
            return null;
        }
    };

    return .{ ._parse = gen.f };
}

pub fn sequence(comptime parsers: anytype) Parser(parsers[0].in(), sequenceOutput(parsers)) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const parsers[0].in()) Parser(parsers[0].in(), sequenceOutput(parsers)).Result {
            var input = _input;
            var output: sequenceOutput(parsers) = undefined;
            inline for (parsers, 0..) |parser, i| {
                const result = try parser.parse(allocator, input) orelse return null;
                input = result.input;
                output[i] = result.output;
            }
            return .{
                .input = input,
                .output = output,
            };
        }
    };

    return .{ ._parse = gen.f };
}

fn sequenceOutput(comptime parsers: anytype) type {
    var types: [parsers.len]type = undefined;
    for (parsers, 0..) |parser, i|
        types[i] = parser.out();
    return std.meta.Tuple(&types);
}

pub fn takeWhile1(comptime T: type, comptime cond: fn (T) bool) Parser(T, []const T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Parser(T, []const T).Result {
            const result = try takeWhile0(T, cond).parse(allocator, input) orelse return null;
            return if (result.output.len == 0) null else result;
        }
    };

    return .{ ._parse = gen.f };
}

pub fn takeWhile0(comptime T: type, comptime cond: fn (T) bool) Parser(T, []const T) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const T) Parser(T, []const T).Result {
            _ = allocator;
            var i: usize = 0;
            while (i < input.len and cond(input[i])) : (i += 1) {}
            return .{
                .input = input[i..],
                .output = input[0..i],
            };
        }
    };

    return .{ ._parse = gen.f };
}

pub fn many0(comptime parser: anytype, comptime skip: ?fn (parser.in()) bool) Parser(parser.in(), []parser.out()) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const parser.in()) Parser(parser.in(), []parser.out()).Result {
            var input = _input;
            var output = try allocator.alloc(parser.out(), 0);

            while (input.len != 0) {
                if (skip) |s|
                    input = (try takeWhile0(parser.in(), s).parse(allocator, input)).?.input;
                const result = try parser.parse(allocator, input) orelse break;
                input = result.input;
                output = try allocator.realloc(output, output.len + 1);
                output[output.len - 1] = result.output;
            }

            return .{
                .input = input,
                .output = output,
            };
        }
    };

    return .{ ._parse = gen.f };
}

pub fn delimited0(comptime parser: anytype, comptime delim: anytype) Parser(parser.in(), []parser.out()) {
    const gen = struct {
        fn f(allocator: Allocator, _input: []const parser.in()) Parser(parser.in(), []parser.out()).Result {
            var input = _input;
            var output = try allocator.alloc(parser.out(), 0);

            while (input.len != 0) {
                if (try parser.parse(allocator, input)) |result| {
                    input = result.input;
                    output = try allocator.realloc(output, output.len + 1);
                    output[output.len - 1] = result.output;
                } else break;

                if (try delim.parse(allocator, input)) |result| {
                    input = result.input;
                } else break;
            }

            return .{
                .input = input,
                .output = output,
            };
        }
    };

    return .{ ._parse = gen.f };
}

pub fn drain(comptime parser: anytype) Parser(parser.in(), void) {
    const gen = struct {
        fn f(allocator: Allocator, input: []const parser.in()) Parser(parser.in(), void).Result {
            if (try parser.parse(allocator, input)) |result|
                return .{ .input = result.input, .output = undefined };
            return null;
        }
    };

    return .{ ._parse = gen.f };
}
