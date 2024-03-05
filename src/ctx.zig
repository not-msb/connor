const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Type = lib.Type;


pub const Context = struct {
    allocator: Allocator,
    symbols: StringHashMap(Type),

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
};
