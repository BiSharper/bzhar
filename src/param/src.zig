const std = @import("std");
const fileutil = @import("fileutil");

const Allocator = std.mem.Allocator;

pub const SourceType = union(enum) {
    File,
    Memory,
};

pub const Source = struct {
    alloc: Allocator,
    name: []const u8,
    stype: SourceType,
    clean_fn: *const fn (Allocator, *anyopaque) void,
    context: *anyopaque,

    const FileContext = struct {
        file: std.fs.File,
    };

    const MemoryContext = struct {
        contents: []const u8,
    };

    pub fn file(path: []const u8, alloc: Allocator) !Source {
        const f = std.fs.cwd().openFile(path, .{}) catch |err| switch (err) {
            error.FileNotFound => return error.FileNotFound,
            error.AccessDenied => return error.AccessDenied,
            else => return error.ReadError,
        };

        fileutil.lockFile(f) catch |err| {
            f.close();
            return err;
        };

        const ctx: *FileContext = try alloc.create(FileContext);
        ctx.* = FileContext{
            .file = f,
        };

        return Source{
            .alloc = alloc,
            .name = try alloc.dupe(u8, path),
            .stype = .File,
            .context = @ptrCast(ctx),
            .clean_fn = &cleanupFile,
        };
    }

    pub fn memory(name: []const u8, contents: []const u8, alloc: Allocator) !Source {
        const ctx = try alloc.create(MemoryContext);
        ctx.* = MemoryContext{
            .contents = try alloc.dupe(u8, contents),
        };

        return Source{
            .alloc = alloc,
            .name = try alloc.dupe(u8, name),
            .stype = .Memory,
            .context = @ptrCast(ctx),
            .clean_fn = &cleanupMemory,
        };
    }

    pub fn getContents(self: *const Source, allocator: Allocator) ![]const u8 {
        switch (self.stype) {
            .File => return readFile(allocator, self.context),
            .Memory => return readMemory(allocator, self.context),
        }
    }

    fn readMemory(allocator: Allocator, ctx: *anyopaque) ![]const u8 {
        const mem_ctx: *MemoryContext = @ptrCast(@alignCast(ctx));
        return try allocator.dupe(u8, mem_ctx.contents);
    }

    fn readFile(allocator: Allocator, ctx: *anyopaque) ![]const u8 {
        const file_ctx: *FileContext = @ptrCast(@alignCast(ctx));

        const f = file_ctx.file;

        f.seekTo(0) catch return error.ReadError;

        const size = try f.getEndPos();
        const contents = try allocator.alloc(u8, size);

        f.seekTo(0) catch {
            allocator.free(contents);
            return error.ReadError;
        };

        const reader = f.reader();
        reader.readNoEof(contents) catch {
            allocator.free(contents);
            return error.ReadError;
        };

        return contents;
    }

    fn cleanupFile(alloc: Allocator, ctx: *anyopaque) void {
        const file_ctx: *FileContext = @ptrCast(@alignCast(ctx));

        fileutil.unlockFile(file_ctx.file);
        file_ctx.file.close();

        alloc.destroy(file_ctx);
    }

    fn cleanupMemory(alloc: Allocator, ctx: *anyopaque) void {
        const mem_ctx: *MemoryContext = @ptrCast(@alignCast(ctx));
        alloc.free(mem_ctx.contents);
        alloc.destroy(mem_ctx);
    }

    pub fn deinit(self: *Source) void {
        self.clean_fn(self.alloc, self.context);
        self.alloc.free(self.name);
    }

    pub fn cloneToAllocator(self: *const Source, alloc: Allocator) !Source {
        return switch (self.stype) {
            .File => try Source.file(self.name, alloc),
            .Memory => blk: {
                const contents = try self.getContents(alloc);
                defer alloc.free(contents);
                break :blk try Source.memory(self.name, contents, alloc);
            },
        };
    }
};
