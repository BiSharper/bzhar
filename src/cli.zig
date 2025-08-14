const std = @import("std");

const Allocator = std.mem.Allocator;
const param = @import("param");
const lzss = @import("lzss");

// Core Types and Structures

pub const Handler = *const fn (*Shell, []const []const u8) anyerror!void;

pub const CommandSpec = struct {
    name: []const u8,
    help: []const u8,
    handler: Handler,
};

pub const ModuleSpec = struct {
    name: []const u8,
    prompt: []const u8,
    commands: []const CommandSpec,
};

// Command Definitions

pub const Modules = &[_]ModuleSpec{
    .{ .name = "lzss", .prompt = "lzss> ", .commands = lzssCommands },
    .{ .name = "param", .prompt = "param> ", .commands = paramCommands },
};

pub const lzssCommands = &[_]CommandSpec{
    .{ .name = "compress", .help = "[text|file]=value <path>", .handler = compressCommand },
    .{ .name = "drycompress", .help = "[text|file]=value count [--roundtrip] [--output-errored=path]", .handler = dryCompressCommand },
};

pub const paramCommands = &[_]CommandSpec{
    .{ .name = "config", .help = "create/delete/select [context_path]", .handler = paramConfigCommand },
    .{ .name = "context", .help = "create/delete/select/list [name] [path]", .handler = paramContextCommand },
    .{ .name = "load", .help = "<file_path> [config_name]", .handler = paramLoadCommand },
    .{ .name = "dryload", .help = "<file_path> [--benchmark=N]", .handler = paramDryloadCommand },
    .{ .name = "list", .help = "<configs|contexts>", .handler = paramListCommand },
    .{ .name = "status", .help = "", .handler = paramStatusCommand },
    .{ .name = "source", .help = "[context_path]", .handler = paramSourceCommand },
};

// State Management

const DEFAULT_CONFIG_NAME: []const u8 = "default";
const ROOT_CONTEXT_NAME: []const u8 = "<none>";

const State = struct {
    configs: std.StringHashMap(*param.Root),
    selected_config: ?[]const u8,
    selected_context: ?[]const u8,
    selected_context_ptr: ?*param.Context,
    allocator: Allocator,

    pub fn init(allocator: Allocator) State {
        return .{
            .configs = std.StringHashMap(*param.Root).init(allocator),
            .selected_config = null,
            .selected_context = null,
            .selected_context_ptr = null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *State) void {
        var iter = self.configs.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.*.release();
            self.allocator.free(entry.key_ptr.*);
        }
        self.configs.deinit();

        if (self.selected_context_ptr) |ptr| {
            ptr.release();
        }
    }

    pub fn getOrCreateDefaultConfig(self: *State) !*param.Root {
        if (self.selected_config) |config_name| {
            if (self.configs.get(config_name)) |root| {
                return root;
            }
        }

        const default_name = DEFAULT_CONFIG_NAME;
        if (self.configs.get(default_name)) |root| {
            self.selected_config = default_name;
            return root;
        }

        const root = try param.Root.create(default_name, self.allocator);
        const name_copy = try self.allocator.dupe(u8, default_name);
        try self.configs.put(name_copy, root);
        self.selected_config = default_name;
        return root;
    }

    pub fn selectConfig(self: *State, name: []const u8) !void {
        if (self.configs.get(name)) |root| {
            self.selected_config = name;

            self.selected_context = ROOT_CONTEXT_NAME;
            if (self.selected_context_ptr) |ptr| {
                ptr.release();
            }
            self.selected_context_ptr = root.retain();
        } else {
            return error.ConfigNotFound;
        }
    }

    pub fn createConfig(self: *State, name: []const u8) !void {
        if (self.configs.get(name)) |_| {
            return error.ConfigAlreadyExists;
        }

        const root = try param.Root.create(name, self.allocator);
        const name_copy = try self.allocator.dupe(u8, name);
        try self.configs.put(name_copy, root);

        if (self.selected_config == null) {
            self.selected_config = name;
            self.selected_context = ROOT_CONTEXT_NAME;
            self.selected_context_ptr = root.retain();
        }
    }

    pub fn deleteConfig(self: *State, name: []const u8) !void {
        if (self.configs.get(name)) |root| {
            root.release();
            _ = self.configs.remove(name);

            if (self.selected_config) |selected| {
                if (std.mem.eql(u8, selected, name)) {
                    self.selected_config = null;
                    self.selected_context = null;
                    if (self.selected_context_ptr) |p| {
                        p.release();
                        self.selected_context_ptr = null;
                    }
                }
            }
        } else {
            return error.ConfigNotFound;
        }
    }
};

// Utility Functions

fn printUsage(shell: *Shell, usage: []const u8) !void {
    try shell.stdout.print("Usage: {s}\n", .{usage});
}

fn printError(shell: *Shell, comptime fmt: []const u8, args: anytype) !void {
    try shell.stdout.print("Error: " ++ fmt ++ "\n", args);
}

fn printSuccess(shell: *Shell, comptime fmt: []const u8, args: anytype) !void {
    try shell.stdout.print(fmt ++ "\n", args);
}

fn navigateToContext(root_ctx: *param.Context, path: []const u8) ?*param.Context {
    if (std.mem.indexOfScalar(u8, path, '.')) |_| {
        var path_parts = std.mem.splitScalar(u8, path, '.');
        var current = root_ctx;

        while (path_parts.next()) |segment| {
            if (segment.len == 0) continue;

            current.rw_lock.lockShared();
            const child_opt = current.children.get(segment);
            current.rw_lock.unlockShared();

            if (child_opt) |child| {
                current = child;
            } else {
                return null;
            }
        }
        return current;
    } else {
        root_ctx.rw_lock.lockShared();
        defer root_ctx.rw_lock.unlockShared();
        return root_ctx.children.get(path);
    }
}

// Command Implementations

fn compressCommand(shell: *Shell, args: []const []const u8) !void {
    if (args.len == 0) {
        try printUsage(shell, "compress <text|file> <path>");
        return;
    }
    // TODO: Implement compression logic
    try printSuccess(shell, "Compression not yet implemented", .{});
}

fn dryCompressCommand(shell: *Shell, args: []const []const u8) !void {
    if (args.len < 2) {
        try printUsage(shell, "drycompress [text|file]=value count [--roundtrip] [--output-errored=path]");
        return;
    }

    const input_spec = args[0];
    const count_str = args[1];
    
    var roundtrip = false;
    var output_errored_path: ?[]const u8 = null;
    
    // Parse optional flags
    var arg_idx: usize = 2;
    while (arg_idx < args.len) : (arg_idx += 1) {
        const arg = args[arg_idx];
        if (std.mem.eql(u8, arg, "--roundtrip")) {
            roundtrip = true;
        } else if (std.mem.startsWith(u8, arg, "--output-errored=")) {
            output_errored_path = arg["--output-errored=".len..];
        } else {
            try printError(shell, "Unknown option: {s}", .{arg});
            return;
        }
    }

    const count = std.fmt.parseInt(usize, count_str, 10) catch {
        try printError(shell, "Invalid count value: {s}", .{count_str});
        return;
    };

    if (count == 0) {
        try printError(shell, "Count must be greater than 0", .{});
        return;
    }

    // Parse [text|file]=value format
    const equals_pos = std.mem.indexOfScalar(u8, input_spec, '=');
    if (equals_pos == null) {
        try printError(shell, "Invalid format. Use '[text|file]=value'", .{});
        return;
    }

    const input_type = input_spec[0..equals_pos.?];
    const contents = input_spec[equals_pos.? + 1 ..];

    var input_data: []const u8 = undefined;
    var is_file = false;
    var file_buffer: ?[]u8 = null;

    if (std.ascii.eqlIgnoreCase(input_type, "file")) {
        is_file = true;
        // Read file contents
        const file = std.fs.cwd().openFile(contents, .{}) catch |err| {
            try printError(shell, "Failed to open file '{s}': {s}", .{ contents, @errorName(err) });
            return;
        };
        defer file.close();

        const stat = file.stat() catch |err| {
            try printError(shell, "Failed to stat file '{s}': {s}", .{ contents, @errorName(err) });
            return;
        };

        file_buffer = shell.allocator.alloc(u8, stat.size) catch |err| {
            try printError(shell, "Failed to allocate memory for file: {s}", .{@errorName(err)});
            return;
        };
        defer if (file_buffer) |buf| shell.allocator.free(buf);

        const bytes_read = file.readAll(file_buffer.?) catch |err| {
            try printError(shell, "Failed to read file '{s}': {s}", .{ contents, @errorName(err) });
            return;
        };

        if (bytes_read != stat.size) {
            try printError(shell, "Failed to read entire file. Expected {d} bytes, got {d}", .{ stat.size, bytes_read });
            return;
        }

        input_data = file_buffer.?;
    } else if (std.ascii.eqlIgnoreCase(input_type, "text")) {
        input_data = contents;
    } else {
        try printError(shell, "Invalid input type: {s}. Use 'text' or 'file'", .{input_type});
        return;
    }

    var totals_ns: u128 = 0;
    var min_ns: u128 = std.math.maxInt(u128);
    var last_compressed_size: usize = 0;

    var i: usize = 0;
    while (i < count) : (i += 1) {
        var timer = try std.time.Timer.start();

        const compressed = lzss.encode(shell.allocator, input_data, false) catch |err| {
            try printError(shell, "Compression failed: {s}", .{@errorName(err)});
            return;
        };
        defer shell.allocator.free(compressed);

        if (roundtrip) {
            const decompressed = lzss.decode(shell.allocator, compressed, input_data.len, false) catch |err| {
                try printError(shell, "Decompression failed: {s}", .{@errorName(err)});
                return;
            };
            defer shell.allocator.free(decompressed);

            // Verify roundtrip integrity
            if (!std.mem.eql(u8, input_data, decompressed)) {
                if (output_errored_path) |path| {
                    // Write the mismatched decompressed data to the specified file
                    const file = std.fs.cwd().createFile(path, .{}) catch |err| {
                        try printError(shell, "Failed to create output file '{s}': {s}", .{ path, @errorName(err) });
                        return;
                    };
                    defer file.close();
                    
                    file.writeAll(decompressed) catch |err| {
                        try printError(shell, "Failed to write to output file '{s}': {s}", .{ path, @errorName(err) });
                        return;
                    };
                    
                    try printError(shell, "Roundtrip verification failed: data mismatch. Decompressed data written to '{s}'", .{path});
                } else {
                    try printError(shell, "Roundtrip verification failed: data mismatch", .{});
                }
                return;
            }
        }

        last_compressed_size = compressed.len;
        const elapsed_ns = timer.read();
        totals_ns += elapsed_ns;
        if (elapsed_ns < min_ns) min_ns = elapsed_ns;
    }

    if (count > 1) {
        const avg_ns = totals_ns / count;
        const compression_ratio = if (input_data.len > 0)
            @as(f64, @floatFromInt(last_compressed_size)) / @as(f64, @floatFromInt(input_data.len))
        else
            0.0;

        const roundtrip_text = if (roundtrip) " + roundtrip" else "";
        try printSuccess(shell, "[drycompress] {d} iters  min {d} ms, avg {d} ms, ratio {d:.3}{s}", .{
            count,
            @as(u64, @intCast(min_ns / 1_000_000)),
            @as(u64, @intCast(avg_ns / 1_000_000)),
            compression_ratio,
            roundtrip_text,
        });
    } else {
        const compression_ratio = if (input_data.len > 0)
            @as(f64, @floatFromInt(last_compressed_size)) / @as(f64, @floatFromInt(input_data.len))
        else
            0.0;

        const roundtrip_text = if (roundtrip) " + roundtrip" else "";
        try printSuccess(shell, "[drycompress] compressed {s} (ratio: {d:.3}){s}", .{ if (is_file) contents else "text", compression_ratio, roundtrip_text });
    }
}

fn paramConfigCommand(shell: *Shell, args: []const []const u8) !void {
    if (args.len < 2) {
        try printUsage(shell, "config <create|delete|select> [name] [context_path]");
        return;
    }

    const action = args[0];
    const name = args[1];
    const context_path = if (args.len > 2) args[2] else null;

    const state = shell.getState();

    if (std.ascii.eqlIgnoreCase(action, "create")) {
        state.createConfig(name) catch |err| switch (err) {
            error.ConfigAlreadyExists => try printError(shell, "Config '{s}' already exists", .{name}),
            else => return err,
        };
        try printSuccess(shell, "Created config: {s}", .{name});
    } else if (std.ascii.eqlIgnoreCase(action, "delete")) {
        state.deleteConfig(name) catch |err| switch (err) {
            error.ConfigNotFound => try printError(shell, "Config '{s}' not found", .{name}),
            else => return err,
        };
        try printSuccess(shell, "Deleted config: {s}", .{name});
    } else if (std.ascii.eqlIgnoreCase(action, "select")) {
        state.selectConfig(name) catch |err| switch (err) {
            error.ConfigNotFound => try printError(shell, "Config '{s}' not found", .{name}),
            else => return err,
        };
        try printSuccess(shell, "Selected config: {s}", .{name});

        if (context_path) |path| {
            const root = state.configs.get(name).?;
            const root_ctx = root.retain();
            defer root_ctx.release();

            if (navigateToContext(root_ctx, path)) |target_ctx| {
                state.selected_context = path;
                state.selected_context_ptr = target_ctx.retain();
                try printSuccess(shell, "Selected context: {s}", .{path});
            } else {
                try printError(shell, "Context path '{s}' not found in config '{s}'", .{ path, name });
            }
        }
    } else {
        try printError(shell, "Unknown action: {s}. Use create, delete, or select", .{action});
    }
}

fn paramContextCommand(shell: *Shell, args: []const []const u8) !void {
    if (args.len < 1) {
        try printUsage(shell, "context <create|delete|select|list> [name]");
        return;
    }

    const action = args[0];
    const name = if (args.len > 1) args[1] else "";
    const state = shell.getState();

    if (std.ascii.eqlIgnoreCase(action, "create")) {
        if (name.len == 0) {
            try printUsage(shell, "context create <name>");
            return;
        }

        if (state.selected_config) |_| {
            const parent_ctx = state.selected_context_ptr.?;
            const created_ctx = try parent_ctx.createClass(name, null);
            created_ctx.release();
            try printSuccess(shell, "Created context: {s}", .{name});
        } else {
            try printError(shell, "No config selected. Create a config first.", .{});
        }
    } else if (std.ascii.eqlIgnoreCase(action, "delete")) {
        if (name.len == 0) {
            try printUsage(shell, "context delete <name>");
            return;
        }

        if (state.selected_config) |_| {
            const parent_ctx = state.selected_context_ptr.?;

            parent_ctx.removeClass(name) catch |err| {
                try printError(shell, "Error deleting context '{s}': {}", .{ name, err });
                return;
            };

            if (state.selected_context) |selected| {
                if (std.mem.eql(u8, selected, name)) {
                    state.selected_context = ROOT_CONTEXT_NAME;
                    if (state.selected_context_ptr) |p| {
                        p.release();
                    }
                    const root = state.configs.get(state.selected_config.?).?;
                    state.selected_context_ptr = root.retain();
                }
            }
            try printSuccess(shell, "Deleted context: {s}", .{name});
        } else {
            try printError(shell, "No config selected", .{});
        }
    } else if (std.ascii.eqlIgnoreCase(action, "select")) {
        if (name.len == 0) {
            try printUsage(shell, "context select <name>");
            return;
        }

        if (state.selected_config) |config_name| {
            if (state.configs.get(config_name)) |root| {
                const root_ctx = root.retain();
                defer root_ctx.release();

                if (std.mem.eql(u8, name, "..")) {
                    if (state.selected_context_ptr) |ptr| {
                        if (ptr.parent) |p| {
                            if (state.selected_context_ptr) |old| old.release();
                            state.selected_context_ptr = p.retain();
                            state.selected_context = p.name;
                            try printSuccess(shell, "Selected context: {s}", .{p.name});
                        } else {
                            if (state.selected_context_ptr) |old| old.release();
                            state.selected_context_ptr = null;
                            state.selected_context = ROOT_CONTEXT_NAME;
                            try printSuccess(shell, "Selected context: <root>", .{});
                        }
                    } else {
                        state.selected_context = ROOT_CONTEXT_NAME;
                        try printSuccess(shell, "Selected context: <root>", .{});
                    }
                    return;
                }

                if (navigateToContext(root_ctx, name)) |target_ctx| {
                    state.selected_context = name;
                    state.selected_context_ptr = target_ctx.retain();
                    try printSuccess(shell, "Selected context: {s}", .{name});
                } else {
                    try printError(shell, "Context '{s}' not found", .{name});
                }
            }
        } else {
            try printError(shell, "No config selected", .{});
        }
    } else if (std.ascii.eqlIgnoreCase(action, "list")) {
        if (state.selected_config) |_| {
            var target_ctx: *param.Context = undefined;
            var header: []const u8 = undefined;

            if (args.len > 1) {
                const path = args[1];

                if (navigateToContext(state.selected_context_ptr.?, path)) |ctx| {
                    target_ctx = ctx;
                    header = path;
                } else {
                    try printError(shell, "Context path '{s}' not found", .{path});
                    return;
                }
            } else {
                target_ctx = state.selected_context_ptr.?;
                header = if (state.selected_context) |sel_name| sel_name else ROOT_CONTEXT_NAME;
            }

            try printSuccess(shell, "Contexts under {s}:", .{header});

            target_ctx.rw_lock.lockShared();
            defer target_ctx.rw_lock.unlockShared();

            var it = target_ctx.children.iterator();
            var any: bool = false;
            while (it.next()) |entry| {
                any = true;
                const marker = if (state.selected_context) |selected|
                    if (std.mem.eql(u8, selected, entry.key_ptr.*)) " *" else "  "
                else
                    "  ";
                try shell.stdout.print("  {s}{s}\n", .{ marker, entry.key_ptr.* });
            }
            if (!any) try printSuccess(shell, "  <none>", .{});
        } else {
            try printError(shell, "No config selected. Use 'config select <name>' first.", .{});
        }
    } else {
        try printError(shell, "Unknown action: {s}. Use create, delete, select, or list", .{action});
    }
}

fn paramLoadCommand(shell: *Shell, args: []const []const u8) !void {
    if (args.len == 0) {
        try printUsage(shell, "load <file_path> [config_name]");
        return;
    }

    const file_path = args[0];
    const config_path = if (args.len > 1) args[1] else null;
    const state = shell.getState();

    if (config_path) |path| {
        if (std.mem.indexOfScalar(u8, path, '.')) |_| {
            var path_parts = std.mem.splitScalar(u8, path, '.');
            const config_name = path_parts.first();
            const context_name = if (path_parts.next()) |ctx| ctx else null;

            if (state.configs.get(config_name)) |root| {
                if (context_name) |ctx_name| {
                    const root_ctx = root.retain();
                    defer root_ctx.release();

                    if (navigateToContext(root_ctx, ctx_name)) |target_ctx| {
                        var source = try param.src.Source.file(file_path, shell.allocator);
                        defer source.deinit();
                        try target_ctx.parse(source, false);
                        try printSuccess(shell, "Loaded file '{s}' into config '{s}' context '{s}'", .{ file_path, config_name, ctx_name });
                    } else {
                        try printError(shell, "Context '{s}' not found in config '{s}'", .{ ctx_name, config_name });
                    }
                } else {
                    const target_ctx = state.selected_context_ptr.?;
                    var source = try param.src.Source.file(file_path, shell.allocator);
                    defer source.deinit();
                    try target_ctx.parse(source, false);
                    try printSuccess(shell, "Loaded file '{s}' into config '{s}' context '{s}'", .{ file_path, config_name, state.selected_context.? });
                }
            } else {
                try printError(shell, "Config '{s}' not found", .{config_name});
            }
        } else {
            if (state.configs.get(path)) |_| {
                const target_ctx = state.selected_context_ptr.?;
                var source = try param.src.Source.file(file_path, shell.allocator);
                defer source.deinit();
                try target_ctx.parse(source, false);
                try printSuccess(shell, "Loaded file '{s}' into config '{s}' context '{s}'", .{ file_path, path, state.selected_context.? });
            } else {
                try state.createConfig(path);
                const target_ctx = state.selected_context_ptr.?;
                var source = try param.src.Source.file(file_path, shell.allocator);
                defer source.deinit();
                try target_ctx.parse(source, false);
                try printSuccess(shell, "Created config '{s}' and loaded file '{s}'", .{ path, file_path });
            }
        }
    } else {
        const default_name = DEFAULT_CONFIG_NAME;
        if (state.configs.get(default_name)) |_| {
            try printError(shell, "Config '{s}' already exists. Specify a config name or use 'config_name.context_name'.", .{default_name});
            return;
        }

        try state.createConfig(default_name);
        const target_ctx = state.selected_context_ptr.?;
        var source = try param.src.Source.file(file_path, shell.allocator);
        defer source.deinit();
        try target_ctx.parse(source, false);
        try printSuccess(shell, "Created config '{s}' and loaded file '{s}'", .{ default_name, file_path });
    }
}

fn paramDryloadCommand(shell: *Shell, args: []const []const u8) !void {
    if (args.len == 0) {
        try printUsage(shell, "dryload <file_path> [--benchmark=N]");
        return;
    }

    const file_path = args[0];
    var iterations: usize = 1;

    if (args.len > 1) {
        const opt = args[1];
        if (std.mem.startsWith(u8, opt, "--benchmark=")) {
            const num_str = opt["--benchmark=".len..];
            iterations = std.fmt.parseInt(usize, num_str, 10) catch {
                try printError(shell, "Invalid --benchmark value: {s}", .{num_str});
                return;
            };
            if (iterations == 0) iterations = 1;
        } else {
            try printError(shell, "Unknown option: {s}", .{opt});
            return;
        }
    }

    var totals_ns: u128 = 0;
    var min_ns: u128 = std.math.maxInt(u128);

    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        const root = try param.Root.create("__dry__", shell.allocator);
        defer root.release();

        const root_ctx = root.retain();
        defer root_ctx.release();

        var source = try param.src.Source.file(file_path, shell.allocator);
        defer source.deinit();

        var timer = try std.time.Timer.start();
        try root_ctx.parse(source, false);
        const elapsed_ns = timer.read();

        totals_ns += elapsed_ns;
        if (elapsed_ns < min_ns) min_ns = elapsed_ns;
    }

    if (iterations > 1) {
        const avg_ns = totals_ns / iterations;
        try printSuccess(shell, "[dryload] {d} iters  min {d} ms, avg {d} ms", .{
            iterations,
            @as(u64, @intCast(min_ns / 1_000_000)),
            @as(u64, @intCast(avg_ns / 1_000_000)),
        });
    } else {
        try printSuccess(shell, "[dryload] parsed {s}", .{file_path});
    }
}

fn paramListCommand(shell: *Shell, args: []const []const u8) !void {
    if (args.len == 0) {
        try printUsage(shell, "list <configs|contexts>");
        return;
    }

    const list_type = args[0];
    const state = shell.getState();

    if (std.ascii.eqlIgnoreCase(list_type, "configs")) {
        try printSuccess(shell, "Available configs:", .{});
        var config_iter = state.configs.iterator();
        while (config_iter.next()) |entry| {
            const config_name = entry.key_ptr.*;
            const marker = if (state.selected_config) |selected|
                if (std.mem.eql(u8, selected, config_name)) " *" else "  "
            else
                "  ";
            try shell.stdout.print("{s}{s}\n", .{ marker, config_name });
        }

        if (state.configs.count() == 0) {
            try printSuccess(shell, "  <none>", .{});
        }
    } else if (std.ascii.eqlIgnoreCase(list_type, "contexts")) {
        if (state.selected_config) |_| {
            try printSuccess(shell, "Contexts in config '{s}':", .{state.selected_config.?});

            const parent_ctx = state.selected_context_ptr.?;

            parent_ctx.rw_lock.lockShared();
            defer parent_ctx.rw_lock.unlockShared();

            var ctx_iter = parent_ctx.children.iterator();
            while (ctx_iter.next()) |entry| {
                const ctx_name = entry.key_ptr.*;
                const marker = if (state.selected_context) |selected|
                    if (std.mem.eql(u8, selected, ctx_name)) " *" else "  "
                else
                    "  ";
                try shell.stdout.print("  {s}{s}\n", .{ marker, ctx_name });
            }

            if (parent_ctx.children.count() == 0) {
                try printSuccess(shell, "  <none>", .{});
            }
        } else {
            try printError(shell, "No config selected. Use 'config select <name>' first.", .{});
        }
    } else {
        try printError(shell, "Unknown list type: {s}. Use 'configs' or 'contexts'", .{list_type});
    }
}

fn paramStatusCommand(shell: *Shell, args: []const []const u8) !void {
    _ = args;
    const state = shell.getState();

    try printSuccess(shell, "=== Param Module Status ===", .{});
    try printSuccess(shell, "Total configs loaded: {d}", .{state.configs.count()});

    if (state.configs.count() == 0) {
        try printSuccess(shell, "No configs loaded", .{});
        return;
    }

    try printSuccess(shell, "\nConfig Details:", .{});
    var config_iter = state.configs.iterator();
    var total_memory: usize = 0;

    while (config_iter.next()) |entry| {
        const config_name = entry.key_ptr.*;
        const root = entry.value_ptr.*;
        const marker = if (state.selected_config) |selected|
            if (std.mem.eql(u8, selected, config_name)) " *" else "  "
        else
            "  ";

        const root_ctx = root.retain();
        defer root_ctx.release();

        root_ctx.rw_lock.lockShared();
        defer root_ctx.rw_lock.unlockShared();

        const context_count = root_ctx.children.count();
        const param_count = root_ctx.params.count();
        const source_count = root.sources.items.len;

        const estimated_memory = context_count * 1024 + param_count * 256 + source_count * 512;
        total_memory += estimated_memory;

        try shell.stdout.print("{s}{s}:\n", .{ marker, config_name });
        try shell.stdout.print("    Contexts: {d}\n", .{context_count});
        try shell.stdout.print("    Parameters: {d}\n", .{param_count});
        try shell.stdout.print("    Sources: {d}\n", .{source_count});
        try shell.stdout.print("    Est. Memory: ~{d} bytes\n", .{estimated_memory});

        if (std.mem.eql(u8, marker, " *")) {
            if (state.selected_context_ptr) |sel_ctx| {
                const full_path = try sel_ctx.getPath(shell.allocator);
                defer shell.allocator.free(full_path);

                try printSuccess(shell, "    Selected Context Details:", .{});
                try printSuccess(shell, "      Path: {s}", .{full_path});
                try printSuccess(shell, "      Name: {s}", .{sel_ctx.name});
                const parent_name = if (sel_ctx.parent) |p| p.name else "<none>";
                try printSuccess(shell, "      Parent: {s}", .{parent_name});
                const base_name = if (sel_ctx.base) |b| b.name else "<none>";
                try printSuccess(shell, "      Extends: {s}", .{base_name});
                try printSuccess(shell, "      Access: {d}", .{@intFromEnum(sel_ctx.access)});

                sel_ctx.rw_lock.lockShared();
                const child_count = sel_ctx.children.count();
                const param_count_ctx = sel_ctx.params.count();
                sel_ctx.rw_lock.unlockShared();

                try printSuccess(shell, "      Children: {d}", .{child_count});
                try printSuccess(shell, "      Parameters: {d}", .{param_count_ctx});
            } else if (state.selected_context) |ctx_name| {
                try printSuccess(shell, "    Selected Context: {s}", .{ctx_name});
            }
        }
        try printSuccess(shell, "", .{});
    }

    try printSuccess(shell, "Total estimated memory usage: ~{d} bytes (~{d:.1} KB)", .{ total_memory, @as(f64, @floatFromInt(total_memory)) / 1024.0 });
}

fn paramSourceCommand(shell: *Shell, args: []const []const u8) !void {
    if (args.len > 1) {
        try printUsage(shell, "source [context_path]");
        return;
    }

    const state = shell.getState();
    if (state.selected_config == null) {
        try printError(shell, "No config selected. Use 'config select <name>' first.", .{});
        return;
    }

    const config_name = state.selected_config.?;
    const root = state.configs.get(config_name).?;
    const root_ctx = root.retain();
    defer root_ctx.release();

    var target_ctx: *param.Context = root_ctx;
    var needs_release = false;

    if (args.len == 1) {
        const context_path = args[0];
        if (navigateToContext(root_ctx, context_path)) |ctx| {
            target_ctx = ctx.retain();
            needs_release = true;
        } else {
            try printError(shell, "Context path '{s}' not found in config '{s}'", .{ context_path, config_name });
            return;
        }
    } else {
        target_ctx = state.selected_context_ptr.?.retain();
        needs_release = true;
    }

    defer if (needs_release) target_ctx.release();

    const context_path = if (target_ctx == root_ctx)
        "<root>"
    else
        try target_ctx.getPath(shell.allocator);

    defer if (target_ctx != root_ctx) shell.allocator.free(context_path);

    if (target_ctx.source) |source| {
        try printSuccess(shell, "Context '{s}' source: {s}", .{ context_path, source.name });
    } else {
        try printSuccess(shell, "Context '{s}' has no source", .{context_path});
    }
}

// Shell Implementation

const Shell = struct {
    allocator: Allocator,
    stdout: std.fs.File.Writer,
    stdin: std.fs.File.Reader,
    current_module_name: []const u8 = &[_]u8{},
    current_prompt: []const u8 = "module> ",
    cwd: ?[]u8 = null,
    modules: []const ModuleSpec,
    state: State,

    fn init(allocator: Allocator, stdout: std.fs.File.Writer, stdin: std.fs.File.Reader) !Shell {
        var shell = Shell{
            .allocator = allocator,
            .stdout = stdout,
            .stdin = stdin,
            .modules = &Modules.*,
            .state = State.init(allocator),
        };

        shell.cwd = try shell.getCurrentWorkingDir();
        return shell;
    }

    fn deinit(self: *Shell) void {
        if (self.cwd) |cwd| {
            self.allocator.free(cwd);
        }
        self.state.deinit();
    }

    fn getState(self: *Shell) *State {
        return &self.state;
    }

    fn getCurrentWorkingDir(self: *Shell) ![]u8 {
        return std.fs.cwd().realpathAlloc(self.allocator, ".") catch |err| switch (err) {
            error.OutOfMemory => return err,
            else => {
                const fallback = try self.allocator.dupe(u8, "<unknown>");
                return fallback;
            },
        };
    }

    fn run(self: *Shell) !void {
        var current: Mode = .root;

        while (true) {
            try self.printPrompt(current);
            const line_opt = try self.readLine(8192);
            if (line_opt == null) break;

            const should_exit = try self.processLine(line_opt.?, &current);
            self.allocator.free(line_opt.?);

            if (should_exit) return;
        }
    }

    fn processLine(self: *Shell, line: []u8, current: *Mode) !bool {
        var mut_line = line;
        self.stripCrLf(&mut_line);
        if (mut_line.len == 0) return false;

        if (std.mem.indexOfScalar(u8, mut_line, '\t')) |tab_pos| {
            const prefix = mut_line[0..tab_pos];
            try self.printCompletions(current.*, prefix);
            return false;
        }

        if (try self.tryRootCommand(mut_line, current)) |should_exit| {
            return should_exit;
        }

        switch (current.*) {
            .root => {
                try self.stdout.print("Unknown command. Type 'help'.\n", .{});
            },
            .module => {
                const lower = std.mem.trim(u8, mut_line, " ");

                if (std.ascii.eqlIgnoreCase(lower, "back")) {
                    current.* = .root;
                    self.current_module_name = &[_]u8{};
                    self.current_prompt = "module> ";
                    return false;
                }

                var tokens = std.mem.tokenizeAny(u8, mut_line, " \t");
                const cmd = tokens.next() orelse {
                    try self.stdout.print("Unknown command. Type 'help'.\n", .{});
                    return false;
                };

                var args_buf = std.ArrayList([]const u8).init(self.allocator);
                defer args_buf.deinit();
                while (tokens.next()) |a| {
                    const arg_copy = try self.allocator.dupe(u8, a);
                    try args_buf.append(arg_copy);
                }
                const args = try args_buf.toOwnedSlice();
                defer {
                    for (args) |arg| self.allocator.free(arg);
                    self.allocator.free(args);
                }

                if (!(try self.invokeCurrentModule(cmd, args))) {
                    try self.stdout.print("Unknown command. Type 'help'.\n", .{});
                }
            },
        }
        return false;
    }

    fn tryRootCommand(self: *Shell, line: []u8, current: *Mode) !?bool {
        var tokens = std.mem.tokenizeAny(u8, line, " \t");
        const cmd = tokens.next() orelse return null;

        if (std.ascii.eqlIgnoreCase(cmd, "help")) {
            try self.printHelp(current.*);
            return false;
        }

        if (std.ascii.eqlIgnoreCase(cmd, "exit")) {
            return true;
        }

        if (std.ascii.eqlIgnoreCase(cmd, "cd")) {
            var args_buf = std.ArrayList([]const u8).init(self.allocator);
            defer args_buf.deinit();
            while (tokens.next()) |a| {
                const arg_copy = try self.allocator.dupe(u8, a);
                try args_buf.append(arg_copy);
            }
            const args = try args_buf.toOwnedSlice();
            defer {
                for (args) |arg| self.allocator.free(arg);
                self.allocator.free(args);
            }
            try self.rootCd(args);
            return false;
        }

        if (std.ascii.eqlIgnoreCase(cmd, "pwd")) {
            try self.rootPwd();
            return false;
        }

        if (std.ascii.eqlIgnoreCase(cmd, "module")) {
            if (tokens.next()) |name| {
                if (try self.enterModule(name)) {
                    current.* = .module;
                } else {
                    try self.stdout.print("No such module: {s}\n", .{name});
                }
            } else {
                try self.stdout.print("Usage: module <name>\n", .{});
            }
            return false;
        }

        if (try self.enterModule(cmd)) {
            current.* = .module;
            return false;
        }

        return null;
    }

    fn rootCd(self: *Shell, args: []const []const u8) !void {
        if (args.len == 0) {
            try printUsage(self, "cd <directory>");
            return;
        }

        const target = args[0];
        var target_dir = std.fs.cwd().openDir(target, .{}) catch |err| {
            try printError(self, "Error changing directory to '{s}': {}", .{ target, err });
            return;
        };
        defer target_dir.close();

        target_dir.setAsCwd() catch |err| {
            try printError(self, "Error setting '{s}' as current directory: {}", .{ target, err });
            return;
        };

        if (self.cwd) |old_cwd| {
            self.allocator.free(old_cwd);
        }
        self.cwd = try self.getCurrentWorkingDir();
        try printSuccess(self, "Changed to: {s}", .{self.cwd.?});
    }

    fn rootPwd(self: *Shell) !void {
        if (self.cwd) |cwd| {
            try printSuccess(self, "{s}", .{cwd});
        } else {
            try printSuccess(self, "Unknown working directory", .{});
        }
    }

    fn printPrompt(self: *Shell, mode: Mode) !void {
        switch (mode) {
            .root => {
                if (self.cwd) |cwd| {
                    const last_sep = std.mem.lastIndexOfAny(u8, cwd, "/\\") orelse 0;
                    const dir_name = if (last_sep == 0) cwd else cwd[last_sep + 1 ..];
                    try self.stdout.print("bzhar[{s}]> ", .{dir_name});
                } else {
                    try self.stdout.print("bzhar> ", .{});
                }
            },
            .module => {
                if (std.ascii.eqlIgnoreCase(self.current_module_name, "param")) {
                    var prompt_buf = std.ArrayList(u8).init(self.allocator);
                    defer prompt_buf.deinit();

                    try prompt_buf.appendSlice("param");

                    if (self.state.selected_config) |config| {
                        try prompt_buf.appendSlice("[");
                        try prompt_buf.appendSlice(config);

                        if (self.state.selected_context_ptr) |ctx_ptr| {
                            const path = try ctx_ptr.getPath(self.allocator);
                            defer self.allocator.free(path);
                            const first_dot = std.mem.indexOfScalar(u8, path, '.');
                            if (first_dot) |idx| {
                                try prompt_buf.appendSlice(".");
                                try prompt_buf.appendSlice(path[idx + 1 ..]);
                            }
                        } else if (self.state.selected_context) |ctx| {
                            try prompt_buf.appendSlice(".");
                            try prompt_buf.appendSlice(ctx);
                        }

                        try prompt_buf.appendSlice("]");
                    }

                    try prompt_buf.appendSlice("> ");
                    try self.stdout.print("{s}", .{prompt_buf.items});
                } else {
                    try self.stdout.print("{s}", .{self.current_prompt});
                }
            },
        }
    }

    fn printCompletions(self: *Shell, mode: Mode, prefix: []const u8) !void {
        const last_token = getLastToken(prefix);
        try self.stdout.print("\nCompletions:\n", .{});

        var found_any = false;

        const root_commands = self.getRootCommands();
        for (root_commands) |cmd| {
            if (std.mem.startsWith(u8, cmd, last_token)) {
                try self.stdout.print("  {s} (global)\n", .{cmd});
                found_any = true;
            }
        }

        for (self.modules) |m| {
            if (std.mem.startsWith(u8, m.name, last_token)) {
                try self.stdout.print("  {s} (module)\n", .{m.name});
                found_any = true;
            }
        }

        switch (mode) {
            .root => {},
            .module => {
                if (std.mem.startsWith(u8, "back", last_token)) {
                    try self.stdout.print("  back (return to root)\n", .{});
                    found_any = true;
                }
                for (self.modules) |m| {
                    if (std.ascii.eqlIgnoreCase(m.name, self.current_module_name)) {
                        for (m.commands) |c| {
                            if (std.mem.startsWith(u8, c.name, last_token)) {
                                try self.stdout.print("  {s} - {s}\n", .{ c.name, c.help });
                                found_any = true;
                            }
                        }
                    }
                }
            },
        }

        if (!found_any) {
            try self.stdout.print("  <no completions>\n", .{});
        }
    }

    fn readLine(self: *Shell, max: usize) !?[]u8 {
        return try self.stdin.readUntilDelimiterOrEofAlloc(self.allocator, '\n', max);
    }

    fn stripCrLf(self: *Shell, s_ptr: *[]u8) void {
        _ = self;
        var s = s_ptr.*;
        if (s.len > 0 and s[s.len - 1] == '\r') {
            s_ptr.* = s[0 .. s.len - 1];
        }
    }

    fn printHelp(self: *Shell, mode: Mode) !void {
        switch (mode) {
            .root => {
                try self.stdout.print("Root commands:\n  help\n  exit\n  pwd\n  cd <directory>\n  module <name> //[or type module name directly]\n\nModules:\n", .{});
                if (self.modules.len == 0) {
                    try self.stdout.print("  <none>\n", .{});
                } else {
                    for (self.modules) |m| try self.stdout.print("  {s}\n", .{m.name});
                }
            },
            .module => {
                try self.stdout.print("Global commands:\n  help\n  exit\n  pwd\n  cd <directory>\n  back (return to root)\n\n", .{});

                for (self.modules) |m| {
                    if (std.ascii.eqlIgnoreCase(m.name, self.current_module_name)) {
                        try self.stdout.print("Module {s} commands:\n", .{m.name});
                        if (m.commands.len == 0) {
                            try self.stdout.print("  <none>\n", .{});
                        } else {
                            for (m.commands) |c| try self.stdout.print("  {s} {s}\n", .{ c.name, c.help });
                        }
                        return;
                    }
                }
                try self.stdout.print("No module help available\n", .{});
            },
        }
    }

    fn enterModule(self: *Shell, name: []const u8) !bool {
        for (self.modules) |m| {
            if (std.ascii.eqlIgnoreCase(m.name, name)) {
                self.current_module_name = m.name;
                self.current_prompt = m.prompt;
                return true;
            }
        }
        return false;
    }

    fn invokeCurrentModule(self: *Shell, cmd: []const u8, args: []const []const u8) !bool {
        for (self.modules) |m| {
            if (std.ascii.eqlIgnoreCase(m.name, self.current_module_name)) {
                for (m.commands) |c| {
                    if (std.ascii.eqlIgnoreCase(c.name, cmd)) {
                        try c.handler(self, args);
                        return true;
                    }
                }
            }
        }
        return false;
    }

    fn getRootCommands(self: *Shell) []const []const u8 {
        _ = self;
        return &[_][]const u8{ "help", "exit", "cd", "pwd", "module" };
    }
};

// Utility Functions

const Mode = enum { root, module };

fn getLastToken(line: []const u8) []const u8 {
    const trimmed = std.mem.trim(u8, line, " \t");
    if (trimmed.len == 0) return "";

    var it = std.mem.tokenizeAny(u8, trimmed, " \t");
    var last: []const u8 = "";
    while (it.next()) |tok| last = tok;
    return last;
}

// Entry Point
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();
    const in_reader = stdin.reader();

    try stdout.writer().print("Welcome to bzhar CLI. Type 'help' for modules and usage.\n", .{});

    var shell = try Shell.init(allocator, stdout.writer(), in_reader);
    defer shell.deinit();

    if (shell.cwd) |cwd| {
        try stdout.writer().print("Current directory: {s}\n", .{cwd});
    }

    try shell.run();
}
