const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn ObjectPool(comptime T: type) type {
    return struct {
        const Self = @This();

        arena: std.heap.ArenaAllocator,
        free_objects: std.ArrayList(*T),
        total_allocated: usize,

        pub fn init(allocator: Allocator) Self {
            const arena = std.heap.ArenaAllocator.init(allocator);

            return Self{
                .arena = arena,
                .free_objects = std.ArrayList(*T).init(allocator),
                .total_allocated = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.free_objects.deinit();
            self.arena.deinit();
        }

        pub fn acquire(self: *Self) !*T {
            if (self.free_objects.pop()) |obj| {
                return obj;
            } else {
                const obj = try self.arena.allocator().create(T);
                self.total_allocated += 1;
                return obj;
            }
        }

        pub fn release(self: *Self, obj: *T) void {
            self.free_objects.append(obj) catch {
                return;
            };
        }

        pub fn getStats(self: *const Self) struct { total_allocated: usize, free_count: usize } {
            return .{
                .total_allocated = self.total_allocated,
                .free_count = self.free_objects.items.len,
            };
        }
    };
}

pub const StringPool = struct {
    arena: std.heap.ArenaAllocator,
    strings: std.StringHashMap(void),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .strings = std.StringHashMap(void).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.strings.deinit();
        self.arena.deinit();
    }

    pub fn intern(self: *Self, string: []const u8) ![]const u8 {
        const result = try self.strings.getOrPut(string);
        if (result.found_existing) {
            return result.key_ptr.*;
        }

        const arena_allocator = self.arena.allocator();
        const owned_string = try arena_allocator.dupe(u8, string);

        result.key_ptr.* = owned_string;

        return owned_string;
    }

    pub fn get(self: *Self, string: []const u8) ?[]const u8 {
        if (self.strings.getKey(string)) |key| {
            return key;
        }
        return null;
    }

    pub fn contains(self: *Self, string: []const u8) bool {
        return self.strings.contains(string);
    }

    pub fn count(self: *Self) u32 {
        return @intCast(self.strings.count());
    }

    pub fn iterator(self: *Self) std.StringHashMap(void).KeyIterator {
        return self.strings.keyIterator();
    }
};
