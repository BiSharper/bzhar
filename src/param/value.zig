const std = @import("std");
const pools = @import("mempools");
const src = @import("src.zig");

const Allocator = std.mem.Allocator;

pub const Array = struct {
    values: std.ArrayList(Value),

    pub fn push(self: *Array, value: Value) !void {
        try self.values.append(value);
    }

    pub fn init(allocator: Allocator) Array {
        return Array{
            .values = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn getPath(self: *const Array, target_value: *const Value, allocator: Allocator) Allocator.Error!?[]u8 {
        for (self.values.items, 0..) |*item, index| {
            if (item == target_value) {
                return try std.fmt.allocPrint(allocator, "[{d}]", .{index});
            }

            if (item.data == .array) {
                const arr: *Array = &item.data.array;
                if (try arr.getPath(target_value, allocator)) |found| {
                    const out = try std.fmt.allocPrint(allocator, "[{d}]{s}", .{ index, found });
                    allocator.free(found);
                    return out;
                }
            }
        }

        return null;
    }

    pub fn clone(self: *const Array, allocator: Allocator) Allocator.Error!Array {
        var new_array = Array.init(allocator);
        errdefer new_array.deinit(allocator);

        try new_array.values.ensureTotalCapacity(self.values.items.len);
        for (self.values.items) |*item| {
            const cloned_item: Value = try item.clone(allocator);
            new_array.values.appendAssumeCapacity(cloned_item);
        }
        return new_array;
    }

    pub fn deinit(self: *Array, allocator: Allocator) void {
        for (self.values.items) |*item| {
            item.deinit(allocator);
        }
        self.values.deinit();
    }

    fn clear(self: *Array, allocator: Allocator) void {
        for (self.values.items) |*item| {
            item.deinit(allocator);
        }
        self.values.clearAndFree();
    }

    pub fn toSyntax(self: *const Array, allocator: Allocator) Allocator.Error![]u8 {
        var result = std.ArrayList(u8).init(allocator);
        defer result.deinit();

        try result.append('{');

        for (self.values.items, 0..) |item, index| {
            if (index > 0) {
                try result.append(',');
            }
            const next_slice = try item.toSyntax(allocator);
            try result.appendSlice(next_slice);
            allocator.free(next_slice);
        }

        try result.append('}');

        return result.toOwnedSlice();
    }
};

pub const Value = struct {
    source: ?*const src.Source = null,
    data: union(enum) {
        i32: i32,
        i64: i64,
        f32: f32,
        string: []const u8,
        array: Array,
    },

    pub fn getNumber(self: *const Value) ?i64 {
        return switch (self.data) {
            .i32 => |i| @intCast(i),
            .i64 => |i| i,
            .f32 => |i| @intFromFloat(i),
            else => null,
        };
    }

    pub fn clone(self: *const Value, alloc: Allocator) Allocator.Error!Value {
        return switch (self.data) {
            .i32 => |v| Value{ .source = self.source, .data = .{ .i32 = v } },
            .i64 => |v| Value{ .source = self.source, .data = .{ .i64 = v } },
            .f32 => |v| Value{ .source = self.source, .data = .{ .f32 = v } },
            .string => |s| Value{ .source = self.source, .data = .{ .string = s } },
            .array => |arr| Value{ .source = self.source, .data = .{ .array = try arr.clone(alloc) } },
        };
    }

    pub fn deinit(self: *Value, alloc: Allocator) void {
        switch (self.data) {
            .string => {},
            .array => |*arr| arr.deinit(alloc),
            .i32, .i64, .f32 => {},
        }
    }

    pub fn toSyntax(self: *const Value, allocator: Allocator) Allocator.Error![]u8 {
        var result = std.ArrayList(u8).init(allocator);
        defer result.deinit();

        switch (self.data) {
            .i32 => |v| {
                try std.fmt.format(result.writer(), "{d}", .{v});
            },
            .i64 => |v| {
                try std.fmt.format(result.writer(), "{d}", .{v});
            },
            .f32 => |v| {
                try std.fmt.format(result.writer(), "{d:.6}", .{v});
            },
            .string => |s| {
                try result.append('"');
                for (s) |c| {
                    switch (c) {
                        '"' => try result.appendSlice("\"\""),
                        '\n' => try result.appendSlice("\" \\n \""),
                        else => try result.append(c),
                    }
                }
                try result.append('"');
            },
            .array => |arr| {
                const arr_syntax = try arr.toSyntax(allocator);
                defer allocator.free(arr_syntax);
                try result.appendSlice(arr_syntax);
            },
        }

        return result.toOwnedSlice();
    }
};

pub fn genValue(value: anytype, spool: ?*pools.StringPool, alloc: Allocator) !Value {
    const T = @TypeOf(value);

    if (T == Value) {
        return value;
    }

    if (T == Array) {
        return Value{ .data = .{ .array = value } };
    }

    const type_info = @typeInfo(T);
    switch (type_info) {
        .int => |int_info| {
            if (int_info.bits <= 32) {
                return Value{ .data = .{ .i32 = @intCast(value) } };
            } else {
                return Value{ .data = .{ .i64 = @intCast(value) } };
            }
        },
        .comptime_int => {
            if (value <= std.math.maxInt(i32) and value >= std.math.minInt(i32)) {
                return Value{ .data = .{ .i32 = @intCast(value) } };
            } else {
                return Value{ .data = .{ .i64 = @intCast(value) } };
            }
        },
        .float, .comptime_float => {
            return Value{ .data = .{ .f32 = @floatCast(value) } };
        },
        .pointer => |ptr_info| {
            switch (ptr_info.size) {
                .slice => {
                    if (ptr_info.child == u8) {
                        if (spool) |sp| {
                            const val = try sp.intern(value);
                            return Value{ .data = .{ .string = val } };
                        } else {
                            return Value{ .data = .{ .string = try alloc.dupe(u8, value) } };
                        }
                    } else {
                        var array_list = std.ArrayList(Value).init(alloc);
                        errdefer array_list.deinit();
                        for (value) |item| {
                            const item_value = try genValue(item, spool, alloc);
                            try array_list.append(item_value);
                        }
                        return Value{ .data = .{ .array = .{ .values = array_list } } };
                    }
                },
                .many, .one => {
                    const Pointee = ptr_info.child;
                    const pointee_info = @typeInfo(Pointee);

                    if (ptr_info.child == u8) {
                        const len = std.mem.len(value);
                        if (spool) |sp| {
                            return Value{ .data = .{ .string = try sp.intern(value[0..len]) } };
                        } else {
                            return Value{ .data = .{ .string = try alloc.dupe(u8, value[0..len]) } };
                        }
                    }

                    if (pointee_info == .array) {
                        if (pointee_info.array.child == u8) {
                            const slice = &value.*;
                            if (spool) |sp| {
                                return Value{ .data = .{ .string = try sp.intern(slice) } };
                            } else {
                                return Value{ .data = .{ .string = try alloc.dupe(u8, slice) } };
                            }
                        } else {
                            var array_list = std.ArrayList(Value).init(alloc);
                            errdefer array_list.deinit();
                            for (value.*) |item| {
                                const item_value = try genValue(item, spool, alloc);
                                try array_list.append(item_value);
                            }
                            return Value{ .data = .{ .array = .{ .values = array_list } } };
                        }
                    }
                    @compileError("Unsupported pointer to one/many type: " ++ @typeName(T));
                },
                else => @compileError("Unsupported pointer type for parameter value"),
            }
        },
        .array => |array_info| {
            if (array_info.child == u8) {
                if (spool) |sp| {
                    return Value{ .data = .{ .string = try sp.intern(value[0..]) } };
                } else {
                    return Value{ .data = .{ .string = try alloc.dupe(u8, value[0..]) } };
                }
            } else {
                var array_list = std.ArrayList(Value).init(alloc);
                errdefer array_list.deinit();

                for (value) |item| {
                    const item_value = try genValue(item, spool, alloc);
                    try array_list.append(item_value);
                }

                return Value{ .data = .{ .array = .{ .values = array_list } } };
            }
        },
        else => @compileError("Unsupported type for parameter value: " ++ @typeName(T)),
    }
}
