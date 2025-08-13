const std = @import("std");
const pools = @import("mempools");
const values = @import("value.zig");
const ast = @import("ast.zig");
const src = @import("src.zig");

const Allocator = std.mem.Allocator;
const AtomicUsize = std.atomic.Value(usize);

pub const Access = enum(i8) {
    Default = -1,
    ReadWrite = 0,
    ReadCreate = 1,
    ReadOnly = 2,
    ReadOnlyVerified = 3,

    pub fn toSyntax(self: Access, allocator: Allocator) Allocator.Error![]const u8 {
        return try std.fmt.allocPrint(allocator, "access = {d};\n", .{@intFromEnum(self)});
    }
};

pub const Root = struct {
    allocator: Allocator,
    cpool: pools.ObjectPool(Context),
    parpool: pools.ObjectPool(Parameter),
    srcpool: pools.ObjectPool(src.Source),

    spool: pools.StringPool,

    sources: std.ArrayList(*src.Source),

    name: []const u8,
    inner: *Context,

    pub fn genValue(self: *Root, value: anytype) !values.Value {
        return try values.genValue(value, &self.spool, self.allocator);
    }

    pub fn retain(self: *Root) *Context {
        return self.inner.retain();
    }

    pub fn release(self: *Root) void {
        var list = std.ArrayList(*Context).init(self.allocator);
        defer list.deinit();

        self.collectAllContexts(self.inner, &list) catch {};

        for (list.items) |ctx| {
            ctx.extend(null) catch {};
        }

        self.inner.release();
    }

    pub fn create(name: []const u8, allocator: Allocator) !*Root {
        const file = try allocator.create(Root);
        errdefer allocator.destroy(file);

        var param_pool: pools.ObjectPool(Parameter) = pools.ObjectPool(Parameter).init(allocator);
        errdefer param_pool.deinit();

        var context_pool: pools.ObjectPool(Context) = pools.ObjectPool(Context).init(allocator);
        errdefer context_pool.deinit();

        var source_pool: pools.ObjectPool(src.Source) = pools.ObjectPool(src.Source).init(allocator);
        errdefer source_pool.deinit();

        var string_pool: pools.StringPool = pools.StringPool.init(allocator);
        errdefer string_pool.deinit();

        const root_ctx = try context_pool.acquire();
        errdefer context_pool.release(root_ctx);

        const parent_strongs = try allocator.alloc(*AtomicUsize, 1);
        errdefer allocator.free(parent_strongs);

        const name_copy = try string_pool.intern(name);

        file.* = .{
            .allocator = allocator,
            .cpool = context_pool,
            .parpool = param_pool,
            .srcpool = source_pool,
            .spool = string_pool,
            .sources = std.ArrayList(*src.Source).init(allocator),

            .name = name_copy,
            .inner = root_ctx,
        };

        root_ctx.* = .{
            .name = file.name,
            .refs = AtomicUsize.init(1),
            .derivatives = AtomicUsize.init(0),
            .parent_refs = parent_strongs,
            .children = std.StringHashMap(*Context).init(allocator),
            .params = std.StringHashMap(*Parameter).init(allocator),
            .deletions = std.StringHashMap(*const src.Source).init(allocator),
            .root = file,
            .parent = null,
            .base = null,
            .access = .Default,
            .source = null,
        };

        root_ctx.parent_refs[0] = &root_ctx.refs;

        return file;
    }

    pub fn internSource(self: *Root, source: *const src.Source) !*src.Source {
        const pooled = try self.srcpool.acquire();
        errdefer self.srcpool.release(pooled);
        pooled.* = try source.cloneToAllocator(self.allocator);
        try self.sources.append(pooled);
        return pooled;
    }

    pub fn parseFile(self: *Root, path: []const u8, protect: bool) !void {
        const ctx = self.retain();
        defer ctx.release();
        try ctx.parseFile(path, protect);
    }

    pub fn parseText(self: *Root, name: []const u8, contents: []const u8, protect: bool) !void {
        const ctx = self.retain();
        defer ctx.release();
        try ctx.parseText(name, contents, protect);
    }

    pub fn ensureClassPath(self: *Root, dotted: []const u8) !*Context {
        const ctx = self.retain();
        defer ctx.release();
        return try ctx.ensureClassPath(dotted);
    }

    pub fn set(self: *Root, name: []const u8, value: anytype) !void {
        const ctx = self.retain();
        defer ctx.release();
        try ctx.set(name, value);
    }

    pub fn getI32(self: *Root, name: []const u8) ?i32 {
        const ctx = self.retain();
        defer ctx.release();
        return ctx.getI32(name);
    }

    pub fn getI64(self: *Root, name: []const u8) ?i64 {
        const ctx = self.retain();
        defer ctx.release();
        return ctx.getI64(name);
    }

    pub fn getF32(self: *Root, name: []const u8) ?f32 {
        const ctx = self.retain();
        defer ctx.release();
        return ctx.getF32(name);
    }

    pub fn getString(self: *Root, name: []const u8) ?[]const u8 {
        const ctx = self.retain();
        defer ctx.release();
        return ctx.getString(name);
    }

    pub fn getValueByPath(self: *Root, path: []const u8) ?*values.Value {
        const ctx = self.retain();
        defer ctx.release();
        return ctx.getValueByPath(path);
    }

    pub fn getNumberByPath(self: *Root, path: []const u8) ?i64 {
        const ctx = self.retain();
        defer ctx.release();
        return ctx.getNumberByPath(path);
    }

    fn collectAllContexts(self: *Root, context: *Context, list: *std.ArrayList(*Context)) !void {
        try list.append(context);

        context.rw_lock.lockShared();
        defer context.rw_lock.unlockShared();

        var child_it = context.children.valueIterator();
        while (child_it.next()) |child_ptr| {
            try self.collectAllContexts(child_ptr.*, list);
        }
    }
};

pub const Parameter = struct {
    name: []const u8,
    value: values.Value,
    source: ?*const src.Source,

    pub fn toSyntax(self: *Parameter, allocator: Allocator) Allocator.Error![]u8 {
        var result = std.ArrayList(u8).init(allocator);
        defer result.deinit();

        try result.appendSlice(self.name);

        if (self.value.data == .array) {
            try result.appendSlice("[]");
        }

        try result.appendSlice(" = ");

        const value_syntax = try self.value.toSyntax(allocator);
        defer allocator.free(value_syntax);

        try result.appendSlice(value_syntax);
        try result.append(';');

        return result.toOwnedSlice();
    }

    pub fn getPath(self: *const Parameter, target_value: *const values.Value, allocator: Allocator) Allocator.Error!?[]u8 {
        if (&self.value == target_value) {
            return try allocator.dupe(u8, self.name);
        }

        if (self.value.data == .array) {
            if (try self.value.data.array.getPath(target_value, allocator)) |suffix| {
                defer allocator.free(suffix);
                return try std.fmt.allocPrint(allocator, "{s}{s}", .{ self.name, suffix });
            }
            return null;
        }

        return null;
    }
};

pub const Context = struct {
    name: []const u8,

    access: Access,
    root: *Root,
    children: std.StringHashMap(*Context),
    params: std.StringHashMap(*Parameter),
    deletions: std.StringHashMap(*const src.Source),
    base: ?*Context,
    parent: ?*Context,
    source: ?*const src.Source,

    parent_refs: []volatile *AtomicUsize,
    refs: AtomicUsize,
    derivatives: AtomicUsize,
    rw_lock: std.Thread.RwLock = .{},
    removed: bool = false,

    pub const DeletionsIterator = struct {
        ctx: *Context,
        inner: std.StringHashMap(*const src.Source).Iterator,

        pub fn next(self: *DeletionsIterator) ?struct { name: []const u8, source: *const src.Source } {
            if (self.inner.next()) |entry| {
                return .{ .name = entry.key_ptr.*, .source = entry.value_ptr.* };
            }
            return null;
        }

        pub fn deinit(self: *DeletionsIterator) void {
            self.ctx.rw_lock.unlockShared();
        }
    };

    pub fn ensureClassPath(self: *Context, dotted: []const u8) !*Context {
        var current = self.retain();
        errdefer current.release();

        var it = std.mem.splitScalar(u8, dotted, '.');
        while (it.next()) |segment| {
            if (segment.len == 0) continue;
            if (current.retainClass(segment)) |child| {
                current.release();
                current = child;
                continue;
            }

            const created = try current.createClass(segment, null);
            current.release();
            current = created;
        }

        return current;
    }

    pub fn parseFile(self: *Context, path: []const u8, protect: bool) !void {
        var source = try src.Source.file(path, self.root.allocator);
        defer source.deinit();
        try self.parse(source, protect);
    }

    pub fn parseText(self: *Context, name: []const u8, contents: []const u8, protect: bool) !void {
        var source = try src.Source.memory(name, contents, self.root.allocator);
        defer source.deinit();
        try self.parse(source, protect);
    }

    pub fn set(self: *Context, name: []const u8, value: anytype) !void {
        self.createValue(name, value, false) catch |err| switch (err) {
            error.ParameterAlreadyExists => return,
            else => return err,
        };
    }

    pub fn getI32(self: *Context, name: []const u8) ?i32 {
        self.rw_lock.lockShared();
        defer self.rw_lock.unlockShared();
        if (self.params.get(name)) |par| {
            return switch (par.value.data) {
                .i32 => |v| v,
                else => null,
            };
        }
        return null;
    }

    pub fn getI64(self: *Context, name: []const u8) ?i64 {
        self.rw_lock.lockShared();
        defer self.rw_lock.unlockShared();
        if (self.params.get(name)) |par| {
            return switch (par.value.data) {
                .i64 => |v| v,
                .i32 => |v| @as(i64, v),
                else => null,
            };
        }
        return null;
    }

    pub fn getF32(self: *Context, name: []const u8) ?f32 {
        self.rw_lock.lockShared();
        defer self.rw_lock.unlockShared();
        if (self.params.get(name)) |par| {
            return switch (par.value.data) {
                .f32 => |v| v,
                else => null,
            };
        }
        return null;
    }

    pub fn getString(self: *Context, name: []const u8) ?[]const u8 {
        self.rw_lock.lockShared();
        defer self.rw_lock.unlockShared();
        if (self.params.get(name)) |par| {
            return switch (par.value.data) {
                .string => |s| s,
                else => null,
            };
        }
        return null;
    }

    pub fn getDeletionSource(self: *Context, name: []const u8) ?*const src.Source {
        self.rw_lock.lockShared();
        defer self.rw_lock.unlockShared();
        return self.deletions.get(name);
    }

    pub fn deletionsIterator(self: *Context) DeletionsIterator {
        self.rw_lock.lockShared();
        return .{ .ctx = self, .inner = self.deletions.iterator() };
    }

    pub fn getValueByPath(self: *Context, path: []const u8) ?*values.Value {
        const last_dot: ?usize = std.mem.lastIndexOfScalar(u8, path, '.');
        var ctx: *Context = self;
        if (last_dot) |ld| {
            var it = std.mem.splitScalar(u8, path[0..ld], '.');
            var is_first = true;
            while (it.next()) |seg| {
                if (seg.len == 0) continue;
                if (is_first) {
                    is_first = false;
                    if (std.mem.eql(u8, seg, ctx.name)) {
                        continue;
                    }
                }
                ctx.rw_lock.lockShared();
                const next = ctx.children.get(seg);
                ctx.rw_lock.unlockShared();
                if (next) |child| {
                    ctx = child;
                } else {
                    return null;
                }
            }
        }

        const last_segment: []const u8 = if (last_dot) |ld| path[ld + 1 ..] else path;
        if (last_segment.len == 0) return null;

        const first_bracket = std.mem.indexOfScalar(u8, last_segment, '[');
        const base_name: []const u8 = if (first_bracket) |fb| last_segment[0..fb] else last_segment;
        if (base_name.len == 0) return null;

        ctx.rw_lock.lockShared();
        const par_opt = ctx.params.get(base_name);
        ctx.rw_lock.unlockShared();
        if (par_opt == null) return null;
        var value_ptr: *values.Value = &par_opt.?.value;

        if (first_bracket) |fb_idx| {
            var i: usize = fb_idx;
            while (i < last_segment.len) {
                if (last_segment[i] != '[') break;
                i += 1;
                var idx: usize = 0;
                var had_digit = false;
                while (i < last_segment.len and std.ascii.isDigit(last_segment[i])) : (i += 1) {
                    had_digit = true;
                    const d: usize = @intCast(last_segment[i] - '0');
                    idx = idx * 10 + d;
                }
                if (!had_digit) return null;
                if (i >= last_segment.len or last_segment[i] != ']') return null;
                i += 1;

                if (value_ptr.data != .array) return null;
                var arr: *values.Array = &value_ptr.data.array;
                if (idx >= arr.values.items.len) return null;
                value_ptr = &arr.values.items[idx];
            }
        }

        return value_ptr;
    }

    pub fn getNumberByPath(self: *Context, path: []const u8) ?i64 {
        if (self.getValueByPath(path)) |v| return v.getNumber();
        return null;
    }

    pub fn getPath(self: *Context, allocator: Allocator) Allocator.Error![]u8 {
        var path_components = std.ArrayList([]const u8).init(allocator);
        defer path_components.deinit();

        var current: ?*Context = self;
        while (current) |ctx| {
            try path_components.append(ctx.name);
            current = ctx.parent;
        }

        std.mem.reverse([]const u8, path_components.items);

        var total_len: usize = 0;
        for (path_components.items, 0..) |component, i| {
            total_len += component.len;
            if (i < path_components.items.len - 1) total_len += 1;
        }

        var result = try allocator.alloc(u8, total_len);
        var pos: usize = 0;

        for (path_components.items, 0..) |component, i| {
            @memcpy(result[pos .. pos + component.len], component);
            pos += component.len;
            if (i < path_components.items.len - 1) {
                result[pos] = '.';
                pos += 1;
            }
        }

        return result;
    }

    pub fn toSyntax(self: *Context, allocator: Allocator, indent: usize) Allocator.Error![]u8 {
        var result = std.ArrayList(u8).init(allocator);
        defer result.deinit();

        for (0..indent) |_| try result.append(' ');
        try result.appendSlice("class ");
        try result.appendSlice(self.name);

        if (self.base) |base| {
            try result.appendSlice(" : ");
            try result.appendSlice(base.name);
        }

        try result.appendSlice(" {\n");

        if (self.access != .Default) {
            const s = try self.access.toSyntax(allocator);
            defer allocator.free(s);

            for (0..indent) |_| try result.append(' ');
            try result.appendSlice(s);
        }

        {
            self.rw_lock.lockShared();
            defer self.rw_lock.unlockShared();

            var param_it = self.params.valueIterator();
            while (param_it.next()) |param_ptr| {
                const param_syntax = try param_ptr.*.toSyntax(allocator);
                defer allocator.free(param_syntax);
                for (0..indent) |_| try result.append(' ');
                try result.appendSlice(param_syntax);
                try result.appendSlice("\n");
            }

            var child_it = self.children.valueIterator();
            while (child_it.next()) |child_ptr| {
                const child_syntax = try child_ptr.*.toSyntax(allocator, indent + 4);
                defer allocator.free(child_syntax);
                try result.appendSlice(child_syntax);
                try result.appendSlice("\n");
            }
        }

        for (0..indent) |_| try result.append(' ');
        try result.appendSlice("};");

        return result.toOwnedSlice();
    }

    fn getClass(self: *Context, name: []const u8) ?*Context {
        return self.children.get(name);
    }

    pub fn retainClass(self: *Context, name: []const u8) ?*Context {
        if (self.getClass(name)) |child_ctx| {
            return child_ctx.retain();
        }
        return null;
    }

    pub fn retain(self: *Context) *Context {
        var old_refs = self.refs.load(.acquire);
        while (true) {
            if (old_refs == 0) {
                @panic("attempt to retain object with a zero reference count");
            }

            if (self.refs.cmpxchgWeak(
                old_refs,
                old_refs + 1,
                .acq_rel,
                .acquire,
            )) |actual_val| {
                old_refs = actual_val;
            } else break;
        }

        for (1..self.parent_refs.len) |i| {
            _ = @volatileCast(self.parent_refs[i]).rmw(.Add, 1, .acq_rel);
        }

        return self;
    }

    pub fn release(self: *Context) void {
        const old_refs = self.refs.rmw(.Sub, 1, .acq_rel);
        std.debug.assert(old_refs != 0);

        if (old_refs > 1) {
            for (1..self.parent_refs.len) |i| {
                std.debug.assert(@volatileCast(self.parent_refs[i]).rmw(.Sub, 1, .acq_rel) != 0);
            }
        }

        if (old_refs == 1) {
            std.debug.assert(self.derivatives.load(.acquire) == 0);

            return self.deinit();
        }
    }

    pub fn createClass(self: *Context, name: []const u8, extends: ?*Context) !*Context {
        self.rw_lock.lock();
        defer self.rw_lock.unlock();
        return self.createClassWithSourceUnlocked(name, extends, null);
    }

    pub fn createClassWithSource(self: *Context, name: []const u8, extends: ?*Context, src_opt: ?*const src.Source) !*Context {
        self.rw_lock.lock();
        defer self.rw_lock.unlock();
        return self.createClassWithSourceUnlocked(name, extends, src_opt);
    }

    fn createClassWithSourceUnlocked(self: *Context, name: []const u8, extends: ?*Context, src_opt: ?*const src.Source) !*Context {
        const alloc = self.root.allocator;

        const child_ctx = try self.root.cpool.acquire();
        errdefer self.root.cpool.release(child_ctx);

        const parent_strongs = try alloc.alloc(*AtomicUsize, self.parent_refs.len + 1);
        errdefer alloc.free(parent_strongs);

        @memcpy(parent_strongs[1..], self.parent_refs);

        const owned_name = try self.root.spool.intern(name);

        const gop = try self.children.getOrPut(owned_name);
        if (gop.found_existing) {
            return error.NameAlreadyExists;
        }
        errdefer _ = self.children.remove(owned_name);

        child_ctx.* = .{ .name = gop.key_ptr.*, .access = .Default, .refs = AtomicUsize.init(1), .derivatives = AtomicUsize.init(0), .parent_refs = parent_strongs, .children = std.StringHashMap(*Context).init(alloc), .params = std.StringHashMap(*Parameter).init(alloc), .deletions = std.StringHashMap(*const src.Source).init(alloc), .root = self.root, .parent = self, .base = null, .source = src_opt };
        child_ctx.deletions = std.StringHashMap(*const src.Source).init(alloc);

        child_ctx.parent_refs[0] = &child_ctx.refs;
        child_ctx.extendUnlocked(extends) catch |err| {
            _ = self.children.remove(owned_name);
            child_ctx.children.deinit();
            child_ctx.params.deinit();
            return err;
        };

        gop.value_ptr.* = child_ctx;

        return child_ctx.retain();
    }

    pub fn removeClass(self: *Context, name: []const u8) !void {
        self.rw_lock.lock();
        defer self.rw_lock.unlock();
        try self.removeClassWithSourceUnlocked(name, null);
    }

    // Locked remove that records a source for provenance (kept for API symmetry).
    pub fn removeClassWithSource(self: *Context, name: []const u8, src_opt: ?*const src.Source) !void {
        self.rw_lock.lock();
        defer self.rw_lock.unlock();
        try self.removeClassWithSourceUnlocked(name, src_opt);
    }

    fn removeClassUnlocked(self: *Context, name: []const u8) !void {
        return self.removeClassWithSourceUnlocked(name, null);
    }

    fn removeClassWithSourceUnlocked(self: *Context, name: []const u8, src_opt: ?*const src.Source) !void {
        if (self.removed) {
            return error.WaitingOnRemoval;
        }

        if (self.children.get(name)) |child_ctx| {
            if (child_ctx.derivatives.load(.acquire) > 0) {
                return error.ClassHasDerivatives;
            }

            if (self.children.fetchRemove(name)) |removed_entry| {
                removed_entry.value.removed = true;
                removed_entry.value.release();

                // Record provenance for deletion
                const owned_name = try self.root.spool.intern(name);
                if (src_opt) |src_ref| {
                    _ = try self.deletions.put(owned_name, src_ref);
                } else {
                    // If no source, remove any existing record
                    _ = self.deletions.remove(owned_name);
                }

                return;
            }
        }

        return error.ClassNotFound;
    }

    pub fn createValue(self: *Context, name: []const u8, value: anytype, protect: bool) !void {
        self.rw_lock.lock();
        defer self.rw_lock.unlock();
        return self.createValueWithSourceUnlocked(name, value, protect, null);
    }

    pub fn createValueWithSource(self: *Context, name: []const u8, value: anytype, protect: bool, src_opt: ?*const src.Source) !void {
        self.rw_lock.lock();
        defer self.rw_lock.unlock();
        return self.createValueWithSourceUnlocked(name, value, protect, src_opt);
    }

    fn createValueWithSourceUnlocked(self: *Context, name: []const u8, value: anytype, protect: bool, src_opt: ?*const src.Source) !void {
        if (self.removed) {
            return error.WaitingOnRemoval;
        }

        const alloc = self.root.allocator;

        if (std.mem.eql(u8, name, "access")) {
            return error.ReservedParameterName;
        }

        const owned_name = try self.root.spool.intern(name);

        var owned_value = try values.genValue(value, &self.root.spool, alloc);
        errdefer owned_value.deinit(alloc);

        const gop = try self.params.getOrPut(owned_name);
        if (gop.found_existing) {
            if (protect and @intFromEnum(self.access) > @intFromEnum(Access.ReadWrite)) {
                return error.InvalidAccess;
            }

            const found = gop.value_ptr.*;
            found.value.deinit(alloc);
            found.value = owned_value;
            found.source = src_opt;

            return error.ParameterAlreadyExists;
        }

        const par = try self.root.parpool.acquire();
        errdefer self.root.parpool.release(par);

        par.* = .{
            .name = gop.key_ptr.*,
            .value = owned_value,
            .source = src_opt,
        };

        gop.value_ptr.* = par;
    }

    pub fn extend(self: *Context, new_extends: ?*Context) !void {
        self.rw_lock.lock();
        defer self.rw_lock.unlock();
        try self.extendUnlocked(new_extends);
    }

    fn extendUnlocked(self: *Context, new_extends: ?*Context) !void {
        if (self.removed) {
            return error.WaitingOnRemoval;
        }

        if (new_extends) |new| {
            if (new == self) return error.CircularDependency;

            var current: ?*Context = new.base;

            new.rw_lock.lockShared();
            defer new.rw_lock.unlockShared();
            while (current) |ctx| {
                if (ctx == self) {
                    return error.CircularDependency;
                }

                current = ctx.base;
            }
        }

        if (self.base) |old_base| {
            _ = old_base.derivatives.rmw(.Sub, 1, .acq_rel);
            old_base.release();
        }

        if (new_extends) |new| {
            _ = new.derivatives.rmw(.Add, 1, .acq_rel);
            self.base = new.retain();
        } else {
            self.base = null;
        }
    }

    pub fn parse(self: *Context, source: src.Source, protect: bool) !void {
        var ast_file = try ast.AstFile.init(source, &self.root.spool, self.root.allocator);
        defer (&ast_file).deinit(false);
        const src_ref = try self.root.internSource(&source);
        return self.updateWithSource(ast_file.nodes, protect, src_ref);
    }

    pub fn update(self: *Context, nodes: []ast.AstNode, protect: bool) !void {
        self.rw_lock.lock();
        defer self.rw_lock.unlock();
        return self.updateWithSourceUnlocked(nodes, protect, null);
    }

    pub fn updateWithSource(self: *Context, nodes: []ast.AstNode, protect: bool, src_ref: *const src.Source) !void {
        self.rw_lock.lock();
        defer self.rw_lock.unlock();
        return self.updateWithSourceUnlocked(nodes, protect, src_ref);
    }

    pub fn updateUnlocked(self: *Context, nodes: []ast.AstNode, protect: bool) !void {
        return self.updateWithSourceUnlocked(nodes, protect, null);
    }

    fn updateWithSourceUnlocked(self: *Context, nodes: []ast.AstNode, protect: bool, src_opt: ?*const src.Source) !void {
        const access: Access = if (!protect) .ReadWrite else self.access;
        const alloc = self.root.allocator;
        if (@intFromEnum(access) >= @intFromEnum(Access.ReadOnly)) {
            return error.AccessDenied;
        }

        // Pre-reserve rough capacities to reduce hashmap rehashing during bulk updates.
        var class_count: usize = 0;
        var param_count: usize = 0;
        for (nodes) |n| {
            switch (n) {
                .class => class_count += 1,
                .param, .array => param_count += 1,
                .delete => {},
            }
        }
        try self.children.ensureTotalCapacity(@intCast(self.children.count() + class_count));
        try self.params.ensureTotalCapacity(@intCast(self.params.count() + param_count));

        var idx: usize = 0;
        while (idx < nodes.len) : (idx += 1) {
            const node_ptr: *ast.AstNode = &nodes[idx];
            switch (node_ptr.*) {
                .delete => |del_name| {
                    if (@intFromEnum(access) >= @intFromEnum(Access.ReadCreate)) {
                        return error.AccessDenied;
                    }
                    try self.removeClassWithSourceUnlocked(del_name, src_opt);
                },
                .class => |class_node| {
                    var child_ctx: *Context = undefined;
                    if (self.children.get(class_node.name)) |existing| {
                        child_ctx = existing.retain();
                    } else {
                        child_ctx = try self.createClassWithSourceUnlocked(class_node.name, null, src_opt);
                    }

                    if (class_node.extends) |_| {
                        // TODO: resolve extends
                    }
                    var empty_nodes = [_]ast.AstNode{};
                    const child_nodes: []ast.AstNode = class_node.nodes orelse &empty_nodes;

                    try child_ctx.updateWithSourceUnlocked(child_nodes, protect, src_opt);

                    child_ctx.release();
                },
                .param => |*param_node| {
                    if (std.mem.eql(u8, param_node.name, "access")) {
                        if (self.access == .Default) {
                            const parsed_access = switch (param_node.value.data.i32) {
                                0 => Access.ReadWrite,
                                1 => Access.ReadCreate,
                                2 => Access.ReadOnly,
                                3 => Access.ReadOnlyVerified,
                                else => return error.InvalidAccessValue,
                            };
                            self.access = parsed_access;
                        }
                    } else {
                        // Move value out of AST to avoid clone; neutralize in AST so its deinit is no-op.
                        var moved_value = param_node.value;
                        param_node.value = .{ .data = .{ .i32 = 0 } };
                        moved_value.source = moved_value.source orelse src_opt;
                        try self.createValueWithSourceUnlocked(param_node.name, moved_value, protect, src_opt);
                    }
                },
                .array => |*array_node| {
                    if (@intFromEnum(access) >= @intFromEnum(Access.ReadCreate)) {
                        return error.AccessDenied;
                    }

                    switch (array_node.operator) {
                        .Add => {
                            const found: ?*Parameter = self.params.get(array_node.name);
                            if (found) |par| {
                                if (par.value.data != .array) {
                                    return error.TypeMismatch;
                                }

                                // Move items without cloning
                                for (array_node.value.values.items) |*item| {
                                    var moved_item = item.*;
                                    item.* = .{ .data = .{ .i32 = 0 } };
                                    moved_item.source = moved_item.source orelse src_opt;
                                    try par.value.data.array.values.append(moved_item);
                                }
                                // Free the AST array buffer now that items are moved
                                array_node.value.deinit(alloc);
                                array_node.value = values.Array.init(alloc);
                            } else {
                                // Move entire array into new parameter
                                const moved_arr = array_node.value; // value moved
                                array_node.value = values.Array.init(alloc);
                                for (moved_arr.values.items) |*it| it.source = it.source orelse src_opt;
                                try self.createValueWithSourceUnlocked(array_node.name, values.Value{ .data = .{ .array = moved_arr } }, protect, src_opt);
                            }
                        },
                        .Sub => {
                            return error.NotImplemented;
                        },
                        .Assign => {
                            const found: ?*Parameter = self.params.get(array_node.name);
                            if (found) |par| {
                                if (par.value.data != .array) {
                                    return error.TypeMismatch;
                                }
                                par.value.data.array.deinit(alloc);
                                const moved_arr = array_node.value; // value moved
                                array_node.value = values.Array.init(alloc);
                                for (moved_arr.values.items) |*it| it.source = it.source orelse src_opt;
                                par.value.data.array = moved_arr;
                                par.source = src_opt;
                            } else {
                                const moved_arr2 = array_node.value; // value moved
                                array_node.value = values.Array.init(alloc);
                                for (moved_arr2.values.items) |*it| it.source = it.source orelse src_opt;
                                try self.createValueWithSourceUnlocked(array_node.name, values.Value{ .data = .{ .array = moved_arr2 } }, protect, src_opt);
                            }
                        },
                    }
                },
            }
        }
    }

    pub fn deinitParameter(self: *Context, param: *Parameter) void {
        param.value.deinit(self.root.allocator);
        self.root.parpool.release(param);
    }

    pub fn deinit(self: *Context) void {
        std.debug.assert(self.derivatives.load(.acquire) == 0);

        if (self.base) |base| {
            _ = base.derivatives.rmw(.Sub, 1, .acq_rel);
            base.release();
            self.base = null;
        }

        {
            var children_to_deinit = std.ArrayList(*Context).init(self.root.allocator);
            defer children_to_deinit.deinit();

            {
                self.rw_lock.lock();
                defer self.rw_lock.unlock();

                var child_it = self.children.iterator();
                while (child_it.next()) |entry| {
                    children_to_deinit.append(entry.value_ptr.*) catch @panic("OOM in deinit");
                }

                self.children.clearAndFree();
            }

            for (children_to_deinit.items) |child| {
                child.release();
            }
        }

        {
            std.debug.assert(self.children.count() == 0);
            self.children.deinit();

            self.deletions.deinit();

            var param_it = self.params.valueIterator();
            while (param_it.next()) |param_ptr| {
                self.deinitParameter(param_ptr.*);
            }

            self.params.deinit();
        }

        self.root.allocator.free(@volatileCast(self.parent_refs));

        if (self.parent) |parent| {
            if (self.removed) {
                self.root.cpool.release(self);
            } else {
                parent.rw_lock.lock();
                defer parent.rw_lock.unlock();
                _ = parent.children.remove(self.name);
                self.root.cpool.release(self);
            }
        } else {
            const root_ptr = self.root;
            const allocator = self.root.allocator;

            root_ptr.parpool.deinit();
            root_ptr.cpool.release(self);
            root_ptr.cpool.deinit();
            root_ptr.spool.deinit();
            {
                const it = root_ptr.sources.items;
                for (it) |s| {
                    s.deinit();
                }
                root_ptr.sources.deinit();
            }
            root_ptr.srcpool.deinit();

            allocator.destroy(root_ptr);
        }
    }
};
