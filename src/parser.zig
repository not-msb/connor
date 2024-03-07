const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const tools = lib.tools;

pub fn Parser(comptime I: type, comptime O: type, comptime S: type) type {
    return struct {
        _parse: fn (S, []const I) Result,

        const Self = @This();
        pub const Result = tools.getError(S)!?struct {
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

        pub fn s(self: Self) type {
            _ = self;
            return S;
        }

        pub fn parse(self: Self, state: S, input: []const I) Result {
            return self._parse(state, input);
        }
    };
}

pub fn byte(comptime T: type, comptime S: type, comptime source: T) Parser(T, []const T, S) {
    const gen = struct {
        fn f(state: S, input: []const T) Parser(T, []const T, S).Result {
            _ = state;
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

pub fn bytes(comptime T: type, comptime S: type, comptime source: []const T) Parser(T, []const T, S) {
    const gen = struct {
        fn f(state: S, input: []const T) Parser(T, []const T, S).Result {
            _ = state;
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

pub fn tag(comptime T: type, comptime S: type, comptime source: @typeInfo(T).Union.tag_type.?) Parser(T, T, S) {
    const gen = struct {
        fn f(state: S, input: []const T) Parser(T, T, S).Result {
            _ = state;
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

pub fn map(comptime parser: anytype, comptime r_func: anytype) Parser(parser.in(), stripError(returnType(r_func)), parser.s()) {
    const gen = struct {
        fn f(state: parser.s(), input: []const parser.in()) Parser(parser.in(), stripError(returnType(r_func)), parser.s()).Result {
            const result = try parser.parse(state, input) orelse return null;
            return .{
                .input = result.input,
                .output = try r_func(state, result.output),
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

pub fn replace(comptime parser: anytype, comptime repl: anytype) Parser(parser.in(), @TypeOf(repl), parser.s()) {
    const gen = struct {
        fn f(state: parser.s(), input: []const parser.in()) Parser(parser.in(), @TypeOf(repl), parser.s()).Result {
            const result = try parser.parse(state, input) orelse return null;
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
        fn f(state: parsers[0].s(), input: []const parsers[0].in()) @TypeOf(parsers[0]).Result {
            inline for (parsers) |parser|
                if (try parser.parse(state, input)) |result|
                    return result;
            return null;
        }
    };

    return .{ ._parse = gen.f };
}

pub fn sequence(comptime parsers: anytype) Parser(parsers[0].in(), sequenceOutput(parsers), parsers[0].s()) {
    const gen = struct {
        fn f(state: parsers[0].s(), _input: []const parsers[0].in()) Parser(parsers[0].in(), sequenceOutput(parsers), parsers[0].s()).Result {
            var input = _input;
            var output: sequenceOutput(parsers) = undefined;
            inline for (parsers, 0..) |parser, i| {
                const result = try parser.parse(state, input) orelse return null;
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

pub fn takeWhile1(comptime T: type, comptime S: type, comptime cond: fn (T) bool) Parser(T, []const T, S) {
    const gen = struct {
        fn f(state: S, input: []const T) Parser(T, []const T, S).Result {
            const result = try takeWhile0(T, S, cond).parse(state, input) orelse return null;
            return if (result.output.len == 0) null else result;
        }
    };

    return .{ ._parse = gen.f };
}

pub fn takeWhile0(comptime T: type, comptime S: type, comptime cond: fn (T) bool) Parser(T, []const T, S) {
    const gen = struct {
        fn f(state: S, input: []const T) Parser(T, []const T, S).Result {
            _ = state;
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

pub fn many0(comptime parser: anytype, comptime skip: ?fn (parser.in()) bool) Parser(parser.in(), []parser.out(), parser.s()) {
    const gen = struct {
        fn f(state: parser.s(), _input: []const parser.in()) Parser(parser.in(), []parser.out(), parser.s()).Result {
            var input = _input;
            var output = try state.alloc(parser.out(), 0);

            while (input.len != 0) {
                if (skip) |s|
                    input = (try takeWhile0(parser.in(), parser.s(), s).parse(state, input)).?.input;
                const result = try parser.parse(state, input) orelse break;
                input = result.input;
                output = try state.realloc(output, output.len + 1);
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

pub fn drain(comptime parser: anytype) Parser(parser.in(), void, parser.s()) {
    const gen = struct {
        fn f(state: parser.s(), input: []const parser.in()) Parser(parser.in(), void, parser.s()).Result {
            if (try parser.parse(state, input)) |result|
                return .{ .input = result.input, .output = undefined };
            return null;
        }
    };

    return .{ ._parse = gen.f };
}

test byte {
    const input = "aaa";

    const parser = byte(u8, void, 'a');
    const result = try parser.parse(undefined, input);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings(result.?.input, "aa");
    try std.testing.expectEqualStrings(result.?.output, "a");
}

test bytes {
    const input = "aaa";

    const parser = bytes(u8, void, "aa");
    const result = try parser.parse(undefined, input);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings(result.?.input, "a");
    try std.testing.expectEqualStrings(result.?.output, "aa");
}

test map {
    const Token = enum {
        A,
        B,
        C,

        fn from(state: void, b: []const u8) @This() {
            _ = state;
            return switch (b[0]) {
                'a' => .A,
                'b' => .B,
                'c' => .C,
                else => unreachable,
            };
        }
    };
    const input = "abc";

    const parser = map(byte(u8, void, 'a'), Token.from);
    const result = try parser.parse(undefined, input);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings(result.?.input, "bc");
    try std.testing.expect(result.?.output == .A);
}

test replace {
    const Token = enum { A, B, C };
    const input = "abc";

    const parser = replace(byte(u8, void, 'a'), Token.A);
    const result = try parser.parse(undefined, input);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings(result.?.input, "bc");
    try std.testing.expect(result.?.output == .A);
}

test alt {
    const Token = enum { A, B, C };
    const input = "cba";

    const parser = alt(.{
        replace(byte(u8, void, 'a'), Token.A),
        replace(byte(u8, void, 'b'), Token.B),
        replace(byte(u8, void, 'c'), Token.C),
    });
    const result = try parser.parse(undefined, input);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings(result.?.input, "ba");
    try std.testing.expect(result.?.output == .C);
}

test takeWhile0 {
    const input = "000";

    const parser = takeWhile0(u8, void, std.ascii.isAlphabetic);
    const result = try parser.parse(undefined, input);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings(result.?.input, "000");
    try std.testing.expectEqualStrings(result.?.output, "");
}

test takeWhile1 {
    const input = "000";

    const parser = takeWhile1(u8, void, std.ascii.isAlphabetic);
    const result = try parser.parse(undefined, input);
    try std.testing.expect(result == null);
}
