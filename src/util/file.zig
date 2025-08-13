const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

//TODO: remove this todo once we have a proper file locking mechanism
pub fn lockFile(file: std.fs.File) !void {
    switch (builtin.os.tag) {
        .windows => {
            _ = file;
        },
        .linux, .macos, .freebsd, .netbsd, .openbsd => {},
        else => {},
    }
}

pub fn unlockFile(file: std.fs.File) void {
    switch (builtin.os.tag) {
        .windows => {
            _ = file;
        },
        .linux, .macos, .freebsd, .netbsd, .openbsd => {},
        else => {},
    }
}
