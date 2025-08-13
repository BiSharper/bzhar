const std = @import("std");

const Allocator = std.mem.Allocator;

pub const Handler = *const fn (*Shell, [][]const u8) anyerror!void;

pub const CommandSpec = struct {
    name: []const u8,
    help: []const u8,
    handler: Handler,
};

pub const ModuleSpec = struct {
    name: []const u8,
    prompt: []const u8,
    commands: []const CommandSpec,

    pub fn init(allocator: Allocator, name: []const u8, prompt: []const u8, commands: []const CommandSpec) !ModuleSpec {
        return ModuleSpec{
            .name = try allocator.dupe(u8, name),
            .prompt = try allocator.dupe(u8, prompt),
            .commands = try allocator.dupe(CommandSpec, commands),
        };
    }

    pub fn deinit(self: *ModuleSpec, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.prompt);
        allocator.free(self.commands);
    }
};

pub fn defineCommand(comptime name: []const u8, comptime help: []const u8, comptime handler: Handler) CommandSpec {
    return .{ .name = name, .help = help, .handler = handler };
}

pub fn defineModule(comptime name: []const u8, comptime prompt: []const u8, comptime commands: []const CommandSpec) ModuleSpec {
    return .{ .name = name, .prompt = prompt, .commands = commands };
}

fn rootCd(shell: *Shell, args: [][]const u8) !void {
    if (args.len == 0) {
        try shell.stdout.print("Usage: cd <directory>\n", .{});
        return;
    }

    const target = args[0];

    var target_dir = std.fs.cwd().openDir(target, .{}) catch |err| {
        try shell.stdout.print("Error changing directory to '{s}': {}\n", .{ target, err });
        return;
    };
    defer target_dir.close();

    target_dir.setAsCwd() catch |err| {
        try shell.stdout.print("Error setting '{s}' as current directory: {}\n", .{ target, err });
        return;
    };

    if (shell.cwd) |old_cwd| {
        shell.allocator.free(old_cwd);
    }
    shell.cwd = try shell.getCurrentWorkingDir();
    try shell.stdout.print("Changed to: {s}\n", .{shell.cwd.?});
}

fn rootPwd(shell: *Shell, args: [][]const u8) !void {
    _ = args;
    if (shell.cwd) |cwd| {
        try shell.stdout.print("{s}\n", .{cwd});
    } else {
        try shell.stdout.print("Unknown working directory\n", .{});
    }
}

pub const Modules = &[_]ModuleSpec{ defineModule("lzss", "lzss> ", lzssCommands), defineModule("param", "param> ", paramCommands) };

pub const lzssCommands = &[_]CommandSpec{defineCommand("compress", "[text|file]=value <path>", compressCommand)};
pub const paramCommands = &[_]CommandSpec{
    defineCommand("config", "create/delete/select [name]", paramConfigCommand),
    defineCommand("context", "create/delete/select/list [name]", paramContextCommand),
    defineCommand("load", "<file_path> [config_name]", paramLoadCommand),
    defineCommand("dryload", "<file_path> [--benchmark=N]", paramDryloadCommand),
    defineCommand("list", "<configs|contexts>", paramListCommand),
    defineCommand("status", "", paramStatusCommand),
};

fn compressCommand(shell: *Shell, args: [][]const u8) !void {
    if (args.len == 0) {
        try shell.stdout.print("Check help for usage\n", .{});
        return;
    }

    return;
}
const param = @import("param");

pub var Configs: ?*std.StringHashMap(*param.Root) = null;
pub var SelectedConfig: ?[]const u8 = null;
pub var SelectedContext: ?[]const u8 = null;

pub var SelectedContextPtr: ?*param.Context = null;

fn getConfigs() *std.StringHashMap(*param.Root) {
    if (Configs) |configs| {
        return configs;
    }

    Configs = std.heap.page_allocator.create(std.StringHashMap(*param.Root)) catch unreachable;
    Configs.?.* = std.StringHashMap(*param.Root).init(std.heap.page_allocator);
    return Configs.?;
}

fn getParam(alloc: Allocator) !*param.Root {
    const configs = getConfigs();
    if (SelectedConfig) |config_name| {
        if (configs.get(config_name)) |root| {
            return root;
        }
    }

    const default_name = "default";
    if (configs.get(default_name)) |root| {
        SelectedConfig = default_name;
        return root;
    }

    const root = try param.Root.create(default_name, alloc);
    const name_copy = try alloc.dupe(u8, default_name);
    try configs.put(name_copy, root);
    SelectedConfig = default_name;
    return root;
}

fn paramConfigCommand(shell: *Shell, args: [][]const u8) !void {
    if (args.len < 2) {
        try shell.stdout.print("Usage: config <create|delete|select> [name]\n", .{});
        return;
    }

    const action = args[0];
    const name = if (args.len > 1) args[1] else "";

    const configs = getConfigs();

    if (std.ascii.eqlIgnoreCase(action, "create")) {
        if (name.len == 0) {
            try shell.stdout.print("Usage: config create <name>\n", .{});
            return;
        }

        if (configs.get(name)) |_| {
            try shell.stdout.print("Config '{s}' already exists\n", .{name});
            return;
        }

        const root = try param.Root.create(name, shell.allocator);
        const name_copy = try shell.allocator.dupe(u8, name);
        try configs.put(name_copy, root);
        try shell.stdout.print("Created config: {s}\n", .{name});
    } else if (std.ascii.eqlIgnoreCase(action, "delete")) {
        if (name.len == 0) {
            try shell.stdout.print("Usage: config delete <name>\n", .{});
            return;
        }

        if (configs.get(name)) |root| {
            root.release();

            var key_to_free: ?[]const u8 = null;
            var iter = configs.iterator();
            while (iter.next()) |entry| {
                if (std.mem.eql(u8, entry.key_ptr.*, name)) {
                    key_to_free = entry.key_ptr.*;
                    break;
                }
            }

            _ = configs.remove(name);

            if (key_to_free) |key| {
                shell.allocator.free(key);
            }

            if (SelectedConfig) |selected| {
                if (std.mem.eql(u8, selected, name)) {
                    SelectedConfig = null;
                    SelectedContext = null;
                    if (SelectedContextPtr) |p| {
                        p.release();
                        SelectedContextPtr = null;
                    }
                }
            }

            try shell.stdout.print("Deleted config: {s}\n", .{name});
        } else {
            try shell.stdout.print("Config '{s}' not found\n", .{name});
        }
    } else if (std.ascii.eqlIgnoreCase(action, "select")) {
        if (name.len == 0) {
            try shell.stdout.print("Usage: config select <name>\n", .{});
            return;
        }

        if (configs.get(name)) |_| {
            var it = configs.iterator();
            while (it.next()) |entry| {
                if (std.mem.eql(u8, entry.key_ptr.*, name)) {
                    SelectedConfig = entry.key_ptr.*;
                    break;
                }
            }
            SelectedContext = null;
            if (SelectedContextPtr) |p| {
                p.release();
                SelectedContextPtr = null;
            }
            try shell.stdout.print("Selected config: {s}\n", .{name});
        } else {
            try shell.stdout.print("Config '{s}' not found\n", .{name});
        }
    } else {
        try shell.stdout.print("Unknown action: {s}. Use create, delete, or select\n", .{action});
    }
}

fn paramContextCommand(shell: *Shell, args: [][]const u8) !void {
    if (args.len < 1) {
        try shell.stdout.print("Usage: context <create|delete|select|list> [name]\n", .{});
        return;
    }

    const action = args[0];
    const name = if (args.len > 1) args[1] else "";

    if (std.ascii.eqlIgnoreCase(action, "create")) {
        if (name.len == 0) {
            try shell.stdout.print("Usage: context create <name>\n", .{});
            return;
        }

        const configs = getConfigs();

        if (SelectedConfig) |config_name| {
            if (configs.get(config_name)) |root| {
                const root_ctx = root.retain();
                defer root_ctx.release();

                var parent_ctx: *param.Context = root_ctx;
                var needs_release = false;
                if (SelectedContextPtr) |ptr| {
                    parent_ctx = ptr.retain();
                    needs_release = true;
                } else if (SelectedContext) |sel_name| {
                    root_ctx.rw_lock.lockShared();
                    const sel_opt = root_ctx.children.get(sel_name);
                    root_ctx.rw_lock.unlockShared();
                    if (sel_opt) |sel_ctx| {
                        parent_ctx = sel_ctx.retain();
                        needs_release = true;
                    }
                }

                defer if (needs_release) parent_ctx.release();

                const created_ctx = parent_ctx.createClass(name, null) catch |err| {
                    if (err == error.NameAlreadyExists) {
                        try shell.stdout.print("Context '{s}' already exists\n", .{name});
                        return;
                    } else {
                        return err;
                    }
                };
                created_ctx.release();
                try shell.stdout.print("Created context: {s}\n", .{name});
            }
        } else {
            try shell.stdout.print("No config selected. Create a config first.\n", .{});
        }
    } else if (std.ascii.eqlIgnoreCase(action, "delete")) {
        if (name.len == 0) {
            try shell.stdout.print("Usage: context delete <name>\n", .{});
            return;
        }

        const configs = getConfigs();

        if (SelectedConfig) |config_name| {
            if (configs.get(config_name)) |root| {
                const root_ctx = root.retain();
                defer root_ctx.release();
                root_ctx.removeClass(name) catch |err| {
                    try shell.stdout.print("Error deleting context '{s}': {}\n", .{ name, err });
                    return;
                };
                if (SelectedContext) |selected| {
                    if (std.mem.eql(u8, selected, name)) {
                        SelectedContext = null;
                        if (SelectedContextPtr) |p| {
                            p.release();
                            SelectedContextPtr = null;
                        }
                    }
                }
                try shell.stdout.print("Deleted context: {s}\n", .{name});
            }
        } else {
            try shell.stdout.print("No config selected\n", .{});
        }
    } else if (std.ascii.eqlIgnoreCase(action, "select")) {
        if (name.len == 0) {
            try shell.stdout.print("Usage: context select <name>\n", .{});
            return;
        }

        const configs = getConfigs();

        if (SelectedConfig) |config_name| {
            if (configs.get(config_name)) |root| {
                const root_ctx = root.retain();
                defer root_ctx.release();

                var parent_ctx: *param.Context = root_ctx;
                var needs_release = false;
                if (std.mem.eql(u8, name, "..")) {
                    if (SelectedContextPtr) |ptr| {
                        if (ptr.parent) |p| {
                            parent_ctx = p;
                            if (SelectedContextPtr) |old| old.release();
                            SelectedContextPtr = p.retain();
                            SelectedContext = p.name;
                            try shell.stdout.print("Selected context: {s}\n", .{p.name});
                            return;
                        } else {
                            if (SelectedContextPtr) |old| old.release();
                            SelectedContextPtr = null;
                            SelectedContext = null;
                            try shell.stdout.print("Selected context: <root>\n", .{});
                            return;
                        }
                    } else if (SelectedContext) |_| {
                        SelectedContext = null;
                        try shell.stdout.print("Selected context: <root>\n", .{});
                        return;
                    }
                } else if (SelectedContextPtr) |ptr| {
                    parent_ctx = ptr.retain();
                    needs_release = true;
                } else if (SelectedContext) |sel_name| {
                    root_ctx.rw_lock.lockShared();
                    const sel_opt = root_ctx.children.get(sel_name);
                    root_ctx.rw_lock.unlockShared();
                    if (sel_opt) |sel_ctx| {
                        parent_ctx = sel_ctx.retain();
                        needs_release = true;
                    }
                }

                defer if (needs_release) parent_ctx.release();

                parent_ctx.rw_lock.lockShared();
                defer parent_ctx.rw_lock.unlockShared();

                var ctx_it = parent_ctx.children.iterator();
                var found = false;
                while (ctx_it.next()) |entry| {
                    if (std.mem.eql(u8, entry.key_ptr.*, name)) {
                        SelectedContext = entry.key_ptr.*;
                        found = true;
                        break;
                    }
                }
                if (found) {
                    if (SelectedContextPtr) |old| old.release();
                    if (needs_release) {
                        parent_ctx.rw_lock.unlockShared();
                        _ = parent_ctx.retain();
                        parent_ctx.rw_lock.lockShared();
                    }
                    const sel_ptr = parent_ctx.children.get(SelectedContext.?).?;
                    SelectedContextPtr = sel_ptr.retain();
                    try shell.stdout.print("Selected context: {s}\n", .{name});
                } else {
                    try shell.stdout.print("Context '{s}' not found\n", .{name});
                }
            }
        } else {
            try shell.stdout.print("No config selected\n", .{});
        }
    } else if (std.ascii.eqlIgnoreCase(action, "list")) {
        const configs = getConfigs();
        if (SelectedConfig) |config_name| {
            if (configs.get(config_name)) |root| {
                const root_ctx = root.retain();
                defer root_ctx.release();

                var parent_ctx: *param.Context = root_ctx;
                var needs_release = false;
                if (SelectedContext) |sel_name| {
                    root_ctx.rw_lock.lockShared();
                    const sel_opt = root_ctx.children.get(sel_name);
                    root_ctx.rw_lock.unlockShared();
                    if (sel_opt) |sel_ctx| {
                        parent_ctx = sel_ctx.retain();
                        needs_release = true;
                    }
                }
                defer if (needs_release) parent_ctx.release();

                parent_ctx.rw_lock.lockShared();
                defer parent_ctx.rw_lock.unlockShared();

                const header = if (SelectedContext) |sel_name| sel_name else "<root>";
                try shell.stdout.print("Contexts under {s}:\n", .{header});

                var it = parent_ctx.children.iterator();
                var any: bool = false;
                while (it.next()) |entry| {
                    any = true;
                    const marker = if (SelectedContext) |selected|
                        if (std.mem.eql(u8, selected, entry.key_ptr.*)) " *" else "  "
                    else
                        "  ";
                    try shell.stdout.print("  {s}{s}\n", .{ marker, entry.key_ptr.* });
                }
                if (!any) try shell.stdout.print("  <none>\n", .{});
            }
        } else {
            try shell.stdout.print("No config selected. Use 'config select <name>' first.\n", .{});
        }
    } else {
        try shell.stdout.print("Unknown action: {s}. Use create, delete, select, or list\n", .{action});
    }
}

fn paramLoadCommand(shell: *Shell, args: [][]const u8) !void {
    if (args.len == 0) {
        try shell.stdout.print("Usage: load <file_path> [config_name]\n", .{});
        try shell.stdout.print("  <file_path>: Path to file to parse\n", .{});
        try shell.stdout.print("  [config_name]: Optional config name or config.context path\n", .{});
        try shell.stdout.print("    Examples:\n", .{});
        try shell.stdout.print("      load game.cpp                    # Creates 'default' config\n", .{});
        try shell.stdout.print("      load game.cpp my_config         # Creates 'my_config' config\n", .{});
        try shell.stdout.print("      load game.cpp game.settings     # Loads into 'game' config, 'settings' context\n", .{});
        return;
    }

    if (args.len < 1) {
        try shell.stdout.print("Error: File path is required\n", .{});
        return;
    }

    const file_path = args[0];
    const config_path = if (args.len > 1) args[1] else null;

    if (config_path != null) {
        if (std.mem.indexOfScalar(u8, config_path.?, '.')) |_| {
            var path_parts = std.mem.splitScalar(u8, config_path.?, '.');
            const config_name_part = path_parts.first();
            const context_name_part = if (path_parts.next()) |ctx| ctx else null;

            const configs = getConfigs();
            if (configs.get(config_name_part)) |root| {
                if (context_name_part) |ctx_name| {
                    const root_ctx = root.retain();
                    defer root_ctx.release();

                    var target_ctx = root_ctx;
                    root_ctx.rw_lock.lockShared();
                    defer root_ctx.rw_lock.unlockShared();

                    if (root_ctx.children.get(ctx_name)) |ctx| {
                        target_ctx = ctx;
                    } else {
                        try shell.stdout.print("Context '{s}' not found in config '{s}'\n", .{ ctx_name, config_name_part });
                        return;
                    }

                    var source = try param.src.Source.file(file_path, shell.allocator);
                    defer source.deinit();
                    try target_ctx.parse(source, false);

                    try shell.stdout.print("Loaded file '{s}' into config '{s}' context '{s}'\n", .{ file_path, config_name_part, ctx_name });
                } else {
                    root.parseFile(file_path, false) catch |err| {
                        try shell.stdout.print("Error parsing file '{s}': {}\n", .{ file_path, err });
                        return;
                    };
                    try shell.stdout.print("Loaded file '{s}' into config '{s}' root context\n", .{ file_path, config_name_part });
                }
            } else {
                try shell.stdout.print("Config '{s}' not found\n", .{config_name_part});
                return;
            }
        } else {
            const configs = getConfigs();
            if (configs.get(config_path.?)) |_| {
                try shell.stdout.print("Config '{s}' already exists. Use 'config_name.context_name' to load into existing config.\n", .{config_path.?});
                return;
            }

            const root = try param.Root.create(config_path.?, shell.allocator);
            const name_copy = try shell.allocator.dupe(u8, config_path.?);
            try configs.put(name_copy, root);

            root.parseFile(file_path, false) catch |err| {
                try shell.stdout.print("Error parsing file '{s}': {}\n", .{ file_path, err });
                _ = configs.remove(config_path.?);
                shell.allocator.free(name_copy);
                root.release();
                return;
            };

            try shell.stdout.print("Created config '{s}' and loaded file '{s}'\n", .{ config_path.?, file_path });
        }
    } else {
        const default_name = "default";
        const configs = getConfigs();

        if (configs.get(default_name)) |_| {
            try shell.stdout.print("Config '{s}' already exists. Specify a config name or use 'config_name.context_name'.\n", .{default_name});
            return;
        }

        const root = try param.Root.create(default_name, shell.allocator);
        const name_copy = try shell.allocator.dupe(u8, default_name);
        try configs.put(name_copy, root);

        root.parseFile(file_path, false) catch |err| {
            try shell.stdout.print("Error parsing file '{s}': {}\n", .{ file_path, err });
            _ = configs.remove(default_name);
            shell.allocator.free(name_copy);
            root.release();
            return;
        };

        try shell.stdout.print("Created config '{s}' and loaded file '{s}'\n", .{ default_name, file_path });
    }
}

fn paramDryloadCommand(shell: *Shell, args: [][]const u8) !void {
    if (args.len == 0) {
        try shell.stdout.print("Usage: dryload <file_path> [--benchmark=N]\n", .{});
        return;
    }

    const file_path = args[0];
    var iterations: usize = 1;

    if (args.len > 1) {
        const opt = args[1];
        if (std.mem.startsWith(u8, opt, "--benchmark=")) {
            const num_str = opt["--benchmark=".len..];
            iterations = std.fmt.parseInt(usize, num_str, 10) catch {
                try shell.stdout.print("Invalid --benchmark value: {s}\n", .{num_str});
                return;
            };
            if (iterations == 0) iterations = 1;
        } else {
            try shell.stdout.print("Unknown option: {s}\n", .{opt});
            return;
        }
    }

    var totals_ns: u128 = 0;
    var min_ns: u128 = std.math.maxInt(u128);

    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        const root: *param.Root = try param.Root.create("__dry__", shell.allocator);
        defer root.release();

        const root_ctx: *param.Context = root.retain();
        defer root_ctx.release();

        var source_inst = try param.src.Source.file(file_path, shell.allocator);
        defer source_inst.deinit();

        var timer = try std.time.Timer.start();

        try root_ctx.parse(source_inst, false);

        const elapsed_ns: u128 = timer.read();
        totals_ns += elapsed_ns;
        if (elapsed_ns < min_ns) min_ns = elapsed_ns;
    }

    if (iterations > 1) {
        const avg_ns: u128 = totals_ns / iterations;
        try shell.stdout.print("[dryload] {d} iters  min {d} ms, avg {d} ms\n", .{
            iterations,
            @as(u64, @intCast(min_ns / 1_000_000)),
            @as(u64, @intCast(avg_ns / 1_000_000)),
        });
    } else {
        try shell.stdout.print("[dryload] parsed {s}\n", .{file_path});
    }
}

fn paramListCommand(shell: *Shell, args: [][]const u8) !void {
    if (args.len == 0) {
        try shell.stdout.print("Usage: list <configs|contexts>\n", .{});
        return;
    }

    const list_type = args[0];

    if (std.ascii.eqlIgnoreCase(list_type, "configs")) {
        try shell.stdout.print("Available configs:\n", .{});
        const configs = getConfigs();
        var config_iter = configs.iterator();
        while (config_iter.next()) |entry| {
            const config_name = entry.key_ptr.*;
            const marker = if (SelectedConfig) |selected|
                if (std.mem.eql(u8, selected, config_name)) " *" else "  "
            else
                "  ";

            try shell.stdout.print("{s}{s}\n", .{ marker, config_name });
        }

        if (configs.count() == 0) {
            try shell.stdout.print("  <none>\n", .{});
        }
    } else if (std.ascii.eqlIgnoreCase(list_type, "contexts")) {
        if (SelectedConfig) |config_name| {
            const configs = getConfigs();
            if (configs.get(config_name)) |root| {
                try shell.stdout.print("Contexts in config '{s}':\n", .{config_name});
                const root_ctx = root.retain();
                defer root_ctx.release();

                root_ctx.rw_lock.lockShared();
                defer root_ctx.rw_lock.unlockShared();

                var ctx_iter = root_ctx.children.iterator();
                while (ctx_iter.next()) |entry| {
                    const ctx_name = entry.key_ptr.*;
                    const marker = if (SelectedContext) |selected|
                        if (std.mem.eql(u8, selected, ctx_name)) " *" else "  "
                    else
                        "  ";
                    try shell.stdout.print("  {s}{s}\n", .{ marker, ctx_name });
                }

                if (root_ctx.children.count() == 0) {
                    try shell.stdout.print("  <none>\n", .{});
                }
            }
        } else {
            try shell.stdout.print("No config selected. Use 'config select <name>' first.\n", .{});
        }
    } else {
        try shell.stdout.print("Unknown list type: {s}. Use 'configs' or 'contexts'\n", .{list_type});
    }
}

fn paramStatusCommand(shell: *Shell, args: [][]const u8) !void {
    _ = args;

    try shell.stdout.print("=== Param Module Status ===\n", .{});
    const configs = getConfigs();
    try shell.stdout.print("Total configs loaded: {d}\n", .{configs.count()});

    if (configs.count() == 0) {
        try shell.stdout.print("No configs loaded\n", .{});
        return;
    }

    try shell.stdout.print("\nConfig Details:\n", .{});
    var config_iter = configs.iterator();
    var total_memory: usize = 0;

    while (config_iter.next()) |entry| {
        const config_name = entry.key_ptr.*;
        const root = entry.value_ptr.*;
        const marker = if (SelectedConfig) |selected|
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
            if (SelectedContextPtr) |sel_ctx| {
                const full_path = try sel_ctx.getPath(shell.allocator);
                defer shell.allocator.free(full_path);

                try shell.stdout.print("    Selected Context Details:\n", .{});
                try shell.stdout.print("      Path: {s}\n", .{full_path});
                try shell.stdout.print("      Name: {s}\n", .{sel_ctx.name});
                const parent_name = if (sel_ctx.parent) |p| p.name else "<root>";
                try shell.stdout.print("      Parent: {s}\n", .{parent_name});
                const base_name = if (sel_ctx.base) |b| b.name else "<none>";
                try shell.stdout.print("      Extends: {s}\n", .{base_name});
                try shell.stdout.print("      Access: {d}\n", .{@intFromEnum(sel_ctx.access)});

                sel_ctx.rw_lock.lockShared();
                const child_count = sel_ctx.children.count();
                const param_count_ctx = sel_ctx.params.count();
                var sources_set = std.StringHashMap(void).init(shell.allocator);
                defer sources_set.deinit();
                if (sel_ctx.source) |s| {
                    _ = try sources_set.put(s.name, {});
                }
                {
                    var pit = sel_ctx.params.iterator();
                    while (pit.next()) |e| {
                        if (e.value_ptr.*.source) |ps| {
                            _ = try sources_set.put(ps.name, {});
                        }
                    }
                }

                try shell.stdout.print("      Children: {d}\n", .{child_count});
                try shell.stdout.print("      Parameters: {d}\n", .{param_count_ctx});
                try shell.stdout.print("      Sources: {d}\n", .{sources_set.count()});
                {
                    var sit = sources_set.iterator();
                    while (sit.next()) |se| {
                        try shell.stdout.print("        - {s}\n", .{se.key_ptr.*});
                    }
                }

                if (param_count_ctx > 0) {
                    try shell.stdout.print("      Parameters Detail:\n", .{});
                    var pit2 = sel_ctx.params.iterator();
                    while (pit2.next()) |pe| {
                        const par = pe.value_ptr.*;
                        const ps = try par.toSyntax(shell.allocator);
                        defer shell.allocator.free(ps);
                        const src_name = if (par.source) |psrc| psrc.name else "<none>";
                        try shell.stdout.print("        - {s}  [source: {s}]\n", .{ ps, src_name });
                    }
                }

                if (child_count > 0) {
                    try shell.stdout.print("      Child Contexts:\n", .{});
                    var cit = sel_ctx.children.iterator();
                    while (cit.next()) |ce| {
                        try shell.stdout.print("        - {s}\n", .{ce.key_ptr.*});
                    }
                }
                sel_ctx.rw_lock.unlockShared();
            } else if (SelectedContext) |ctx_name| {
                try shell.stdout.print("    Selected Context: {s}\n", .{ctx_name});
                if (context_count > 0) {
                    try shell.stdout.print("    Available contexts:\n", .{});
                    var ctx_iter = root_ctx.children.iterator();
                    while (ctx_iter.next()) |ctx_entry| {
                        const ctx_marker = if (SelectedContext) |selected|
                            if (std.mem.eql(u8, selected, ctx_entry.key_ptr.*)) " *" else "  "
                        else
                            "  ";
                        try shell.stdout.print("      {s}{s}\n", .{ ctx_marker, ctx_entry.key_ptr.* });
                    }
                }
            }
        }
        try shell.stdout.print("\n", .{});
    }

    try shell.stdout.print("Total estimated memory usage: ~{d} bytes (~{d:.1} KB)\n", .{ total_memory, @as(f64, @floatFromInt(total_memory)) / 1024.0 });
}

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

    defer {
        if (Configs) |configs| {
            var config_iter = configs.iterator();
            while (config_iter.next()) |entry| {
                entry.value_ptr.*.release();
                allocator.free(entry.key_ptr.*);
            }
            configs.deinit();

            std.heap.page_allocator.destroy(configs);
        }
    }

    try shell.run();
}

const Shell = struct {
    allocator: Allocator,
    stdout: std.fs.File.Writer,
    stdin: std.fs.File.Reader,
    current_module_name: []const u8 = &[_]u8{},
    current_prompt: []const u8 = "module> ",
    cwd: ?[]u8 = null,
    modules: []const ModuleSpec,

    fn init(allocator: Allocator, stdout: std.fs.File.Writer, stdin: std.fs.File.Reader) !Shell {
        var shell = Shell{
            .allocator = allocator,
            .stdout = stdout,
            .stdin = stdin,
            .modules = &Modules.*,
        };

        shell.cwd = try shell.getCurrentWorkingDir();

        return shell;
    }

    fn deinit(self: *Shell) void {
        if (self.cwd) |cwd| {
            self.allocator.free(cwd);
        }
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
        const current_module_index: usize = 0;

        while (true) {
            try self.printPrompt(current, current_module_index);
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
                while (tokens.next()) |a| try args_buf.append(a);
                const args = try args_buf.toOwnedSlice();
                defer self.allocator.free(args);
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
            while (tokens.next()) |a| try args_buf.append(a);
            const args = try args_buf.toOwnedSlice();
            defer self.allocator.free(args);
            try rootCd(self, args);
            return false;
        }

        if (std.ascii.eqlIgnoreCase(cmd, "pwd")) {
            try rootPwd(self, &[_][]const u8{});
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

    fn printPrompt(self: *Shell, mode: Mode, module_index: usize) !void {
        _ = module_index;
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

                    if (SelectedConfig) |config| {
                        try prompt_buf.appendSlice("[");
                        try prompt_buf.appendSlice(config);

                        if (SelectedContextPtr) |ctx_ptr| {
                            const path = try ctx_ptr.getPath(self.allocator);
                            defer self.allocator.free(path);
                            const first_dot = std.mem.indexOfScalar(u8, path, '.');
                            if (first_dot) |idx| {
                                try prompt_buf.appendSlice(".");
                                try prompt_buf.appendSlice(path[idx + 1 ..]);
                            }
                        } else if (SelectedContext) |ctx| {
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

    fn getRootCommands(self: *Shell) []const []const u8 {
        _ = self;
        return &[_][]const u8{ "help", "exit", "cd", "pwd", "module" };
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

        inline for (Modules) |m| {
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
                inline for (Modules) |m| {
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
                if (Modules.len == 0) {
                    try self.stdout.print("  <none>\n", .{});
                } else {
                    inline for (Modules) |m| try self.stdout.print("  {s}\n", .{m.name});
                }
            },
            .module => {
                try self.stdout.print("Global commands:\n  help\n  exit\n  pwd\n  cd <directory>\n  back (return to root)\n\n", .{});

                inline for (Modules) |m| {
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
        inline for (Modules, 0..) |m, i| {
            _ = i;
            if (std.ascii.eqlIgnoreCase(m.name, name)) {
                self.current_module_name = m.name;
                self.current_prompt = m.prompt;
                return true;
            }
        }
        return false;
    }

    fn invokeCurrentModule(self: *Shell, cmd: []const u8, args: [][]const u8) !bool {
        inline for (Modules) |m| {
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
};

const Mode = enum { root, module };

const RootAction = union(enum) { Help, Exit, Module: []const u8, Unknown };

fn parseRoot(line: []const u8) RootAction {
    var it = std.mem.tokenizeAny(u8, line, " \t");
    const first = it.next() orelse return .Unknown;
    if (std.ascii.eqlIgnoreCase(first, "help")) return .Help;
    if (std.ascii.eqlIgnoreCase(first, "exit")) return .Exit;
    if (std.ascii.eqlIgnoreCase(first, "module")) {
        if (it.next()) |name| return .{ .Module = name };
        return .Unknown;
    }
    return .{ .Module = first };
}

fn getLastToken(line: []const u8) []const u8 {
    const trimmed = std.mem.trim(u8, line, " \t");
    if (trimmed.len == 0) return "";

    var it = std.mem.tokenizeAny(u8, trimmed, " \t");
    var last: []const u8 = "";
    while (it.next()) |tok| last = tok;
    return last;
}
