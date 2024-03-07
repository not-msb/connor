const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Type = lib.Type;

pub const Context = struct {
    allocator: Allocator,
    symbols: StringHashMap(Type),

    pub const Error = Allocator.Error;

    pub fn init(allocator: Allocator) Context {
        return .{
            .allocator = allocator,
            .symbols = StringHashMap(Type).init(allocator),
        };
    }

    pub fn deinit(self: *Context) void {
        self.symbols.deinit();
    }

    pub fn clone(self: *const Context) Allocator.Error!Context {
        return .{
            .allocator = self.allocator,
            .symbols = try self.symbols.clone(),
        };
    }

    pub fn get(self: *const Context, key: []const u8) ?Type {
        return self.symbols.get(key);
    }

    pub fn put(self: *Context, key: []const u8, value: Type) Allocator.Error!void {
        return self.symbols.put(key, value);
    }

    pub fn alloc(self: *const Context, comptime T: type, n: usize) ![]T {
        return self.allocator.alloc(T, n);
    }

    pub fn realloc(self: *const Context, old_mem: anytype, new_n: usize) ![]@typeInfo(@TypeOf(old_mem)).Pointer.child {
        return self.allocator.realloc(old_mem, new_n);
    }
};
