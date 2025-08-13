const std = @import("std");
const pools = @import("mempools");
const src = @import("src.zig");
const values = @import("value.zig");

const Allocator = std.mem.Allocator;
const Array = values.Array;
const Value = values.Value;
const genValue = values.genValue;
const Source = src.Source;

pub const AstFile = struct {
    allocator: Allocator,
    nodes: []AstNode,
    string_pool: *pools.StringPool,
    length: usize,

    pub fn init(source: Source, string_pool: *pools.StringPool, allocator: Allocator) !AstFile {
        var line: usize = 1;
        var index: usize = 0;
        var line_start: usize = index;

        const contents = try source.getContents(allocator);
        defer allocator.free(contents);

        const nodes = try AstNode.parseContext(contents, string_pool, source.name, &index, &line, &line_start, allocator);

        return .{ .allocator = allocator, .nodes = nodes, .string_pool = string_pool, .length = index };
    }

    pub fn deinit(self: *AstFile, deinit_pool: bool) void {
        for (self.nodes) |*node| node.deinit(self.allocator);
        self.allocator.free(self.nodes);
        if (deinit_pool) {
            self.string_pool.deinit();
        }
    }
};

pub const AstNode = union(enum) {
    class: AstClass,
    param: AstParam,
    delete: []const u8,
    array: AstArray,

    pub const AstClass = struct { name: []const u8, extends: ?[]const u8, nodes: ?[]AstNode };

    pub const AstParam = struct {
        name: []const u8,
        value: Value,
    };

    pub const AstOperator = enum {
        Add,
        Sub,
        Assign,
    };

    pub const AstArray = struct {
        name: []const u8,
        operator: AstOperator,
        value: Array,
    };

    pub fn deinit(self: *AstNode, allocator: Allocator) void {
        switch (self.*) {
            .class => |*class_node| {
                if (class_node.nodes) |nodes| {
                    for (nodes) |*node| {
                        node.deinit(allocator);
                    }
                    allocator.free(nodes);
                }
            },
            .param => |*param_node| {
                param_node.value.deinit(allocator);
            },
            .delete => {},
            .array => |*array_node| {
                array_node.value.deinit(allocator);
            },
        }
    }

    fn skipToStmtBoundary(input: []const u8, index: *usize, line: *usize, line_start: *usize) void {
        while (index.* < input.len) : (index.* += 1) {
            const c = input[index.*];
            if (c == ';') {
                index.* += 1;
                break;
            }
            if (c == '}') {
                break;
            }
            if (c == '\n') {
                line.* += 1;
                index.* += 1;
                line_start.* = index.*;
                break;
            }
            if (c == '\r') {}
        }
    }

    fn getUnquotedSlice(input: []const u8, index: *usize, line: *usize, line_start: *usize, terminators: []const u8) []const u8 {
        skipWhitespace(input, index, line, line_start);
        const start = index.*;
        while (index.* < input.len) {
            const c = input[index.*];
            if (std.mem.indexOfScalar(u8, terminators, c) != null) break;
            if (c == '\n') {
                line.* += 1;
                index.* += 1;
                line_start.* = index.*;
                break;
            } else if (c == '\r') {
                index.* += 1;
                break;
            } else {
                index.* += 1;
            }
        }
        return input[start..index.*];
    }

    pub fn parseContext(input: []const u8, spool: *pools.StringPool, src_name: []const u8, index: *usize, line: *usize, line_start: *usize, allocator: Allocator) ![]AstNode {
        var nodes = std.ArrayList(AstNode).init(allocator);
        errdefer {
            for (nodes.items) |*node| node.deinit(allocator);
            nodes.deinit();
        }

        var had_error: bool = false;

        while (index.* < input.len) {
            skipWhitespace(input, index, line, line_start);

            if (index.* >= input.len) {
                break;
            }

            const c = input[index.*];
            if (c == '#') {
                // todo directives
            }

            if (c == '}') {
                index.* += 1;
                if (index.* < input.len and input[index.*] == ';') {
                    index.* += 1;
                } else {
                    std.log.warn("[{s}] Error at line {}, col {}: expected ';' after class ending.", .{ src_name, line.*, index.* - line_start.* });
                    return error.SyntaxError;
                }
                break;
            }
            var word = getAlphaWord(input, index, line, line_start);
            if (std.mem.eql(u8, word, "delete")) {
                word = getAlphaWord(input, index, line, line_start);
                word = try spool.intern(word);

                skipWhitespace(input, index, line, line_start);

                if (input[index.*] != ';') {
                    std.log.warn("[{s}] Error at line {}, col {}: expected ';' after 'delete' statement.", .{ src_name, line.*, index.* - line_start.* });
                    had_error = true;
                    skipToStmtBoundary(input, index, line, line_start);
                } else {
                    index.* += 1;
                }
                try nodes.append(.{ .delete = word });
            } else if (std.mem.eql(u8, word, "class")) {
                const class_name = try spool.intern(getAlphaWord(input, index, line, line_start));

                skipWhitespace(input, index, line, line_start);

                if (input[index.*] == ';') {
                    index.* += 1;
                    try nodes.append(.{ .class = .{ .name = class_name, .extends = null, .nodes = null } });
                } else {
                    var extends_name: ?[]const u8 = null;
                    if (input[index.*] == ':') {
                        index.* += 1;
                        extends_name = getAlphaWord(input, index, line, line_start);
                        extends_name = try spool.intern(extends_name.?);
                    }
                    skipWhitespace(input, index, line, line_start);

                    if (input[index.*] != '{') {
                        if (extends_name != null) {
                            std.log.warn("[{s}] Error at line {}, col {}: expected '{{' after class declaration.", .{ src_name, line.*, index.* - line_start.* });
                        } else {
                            std.log.warn("[{s}] Error at line {}, col {}: expected ';' or '{{' after class declaration.", .{ src_name, line.*, index.* - line_start.* });
                        }
                        had_error = true;
                        skipToStmtBoundary(input, index, line, line_start);
                        continue;
                    }
                    index.* += 1;

                    var child_nodes: ?[]AstNode = null;
                    const nested_result = AstNode.parseContext(input, spool, src_name, index, line, line_start, allocator);
                    if (nested_result) |cn| {
                        child_nodes = cn;
                    } else |err| {
                        if (err == error.SyntaxError) {
                            std.log.warn("[{s}] Error at line {}, col {}: Missing closing brace '}}' for class.", .{ src_name, line.*, index.* - line_start.* });
                            return err;
                        } else {
                            return err;
                        }
                    }

                    try nodes.append(.{ .class = .{ .name = class_name, .extends = extends_name, .nodes = child_nodes } });
                }
            } else {
                word = try spool.intern(word);
                if (input[index.*] == '[') {
                    index.* += 1;

                    skipWhitespace(input, index, line, line_start);

                    if (input[index.*] != ']') {
                        std.log.warn("[{s}] Error at line {}, col {}: expected ']' or whitespace after '['", .{ src_name, line.*, index.* - line_start.* });
                        return error.SyntaxError;
                    }
                    index.* += 1;

                    skipWhitespace(input, index, line, line_start);

                    const operator: AstNode.AstOperator = blk: switch (input[index.*]) {
                        '+' => {
                            index.* += 1;

                            if (input[index.*] != '=') {
                                std.log.warn("[{s}] Error at line {}, col {}: expected '=' after '+'", .{ src_name, line.*, index.* - line_start.* });
                                return error.SyntaxError;
                            }
                            index.* += 1;
                            break :blk AstNode.AstOperator.Add;
                        },
                        '-' => {
                            index.* += 1;

                            if (input[index.*] != '=') {
                                std.log.warn("[{s}] Error at line {}, col {}: expected '=' after '+'", .{ src_name, line.*, index.* - line_start.* });
                                return error.SyntaxError;
                            }
                            index.* += 1;
                            break :blk AstNode.AstOperator.Sub;
                        },
                        '=' => {
                            index.* += 1;
                            break :blk AstNode.AstOperator.Assign;
                        },
                        else => {
                            std.log.warn("[{s}] Error at line {}, col {}: expected '=', '+=', or '-=' after array name", .{ src_name, line.*, index.* - line_start.* });
                            return error.SyntaxError;
                        },
                    };
                    const array = try parseArray(input, spool, src_name, index, line, line_start, allocator);

                    try nodes.append(.{ .array = .{ .name = word, .operator = operator, .value = array } });
                    skipWhitespace(input, index, line, line_start);

                    if (input[index.*] != ';') {
                        std.log.warn("[{s}] Error at line {}, col {}: expected ';' after array parameter", .{ src_name, line.*, index.* - line_start.* });
                        had_error = true;
                        skipToStmtBoundary(input, index, line, line_start);
                    } else {
                        index.* += 1;
                    }
                } else {
                    skipWhitespace(input, index, line, line_start);
                    if (input[index.*] != '=') {
                        std.log.warn("[{s}] Error at line {}, col {}: expected '=' or '[' after parameter name", .{ src_name, line.*, index.* - line_start.* });
                        had_error = true;
                        skipToStmtBoundary(input, index, line, line_start);
                        continue;
                    }
                    index.* += 1;

                    skipWhitespace(input, index, line, line_start);

                    var expression = false;
                    if (input[index.*] == '@') {
                        expression = true;
                    }

                    var foundQuote: bool = false;
                    var word_slice: []const u8 = &[_]u8{};
                    if (input[index.*] == '"') {
                        const w = try getWord(input, src_name, index, line, line_start, &[_]u8{ ';', '}', '\n', '\r' }, &foundQuote, allocator);
                        word_slice = w;
                    } else {
                        word_slice = getUnquotedSlice(input, index, line, line_start, &[_]u8{ ';', '}', '\n', '\r' });
                        foundQuote = false;
                    }

                    if (index.* >= input.len) {
                        std.log.warn("[{s}] Error at line {}, col {}: Missing ';' at end of parameter.", .{ src_name, line.*, index.* - line_start.* });
                        had_error = true;
                        break;
                    } else switch (input[index.*]) {
                        '}' => {
                            index.* -= 1;
                            std.log.warn("[{s}] Error at line {}, col {}: Missing ';' prior to '}}'", .{ src_name, line.*, index.* - line_start.* });
                            had_error = true;
                        },
                        ';' => {
                            index.* += 1;
                        },
                        else => {
                            const c2 = input[index.*];
                            if (c2 != '\n' and c2 != '\r') {
                                if (!foundQuote) {
                                    std.log.warn("[{s}] Error at line {}, col {}: Expected ';' after parameter value", .{ src_name, line.*, index.* - line_start.* });
                                    had_error = true;
                                } else {
                                    index.* -= 1;
                                }
                            }
                            std.log.warn("[{s}] Warning at line {}, col {}: Missing ';' at end of line.", .{ src_name, line.*, index.* - line_start.* });
                        },
                    }

                    if (expression) {
                        std.log.warn("[{s}] Error at line {}, col {}: Expressions are not yet implemented", .{ src_name, line.*, index.* - line_start.* });
                        return error.NotImplemented;
                    } else if (!foundQuote) {
                        if (word_slice.len > 7 and std.mem.eql(u8, word_slice[0..6], "__EVAL")) {
                            std.log.warn("[{s}] Error at line {}, col {}: Evaluate not yet implemented", .{ src_name, line.*, index.* - line_start.* });
                            return error.NotImplemented;
                        }
                        var value = try scanInt(word_slice, allocator) orelse
                            try scanInt64(word_slice, allocator) orelse
                            try scanFloat(word_slice, allocator) orelse
                            try genValue(word_slice, spool, allocator);
                        errdefer value.deinit(allocator);

                        try nodes.append(.{ .param = .{ .name = word, .value = value } });
                    } else {
                        var value = try genValue(word_slice, spool, allocator);
                        errdefer value.deinit(allocator);

                        try nodes.append(.{ .param = .{ .name = word, .value = value } });
                        allocator.free(word_slice);
                    }
                }
            }
        }

        if (had_error) {
            return error.SyntaxError;
        }
        return try nodes.toOwnedSlice();
    }

    fn parseArray(input: []const u8, spool: *pools.StringPool, src_name: []const u8, index: *usize, line: *usize, line_start: *usize, allocator: Allocator) !Array {
        skipWhitespace(input, index, line, line_start);
        var array = Array.init(allocator);
        errdefer {
            for (array.values.items) |*val| val.deinit(allocator);
            array.deinit(allocator);
        }
        if (input[index.*] != '{') {
            return error.SyntaxError;
        }
        index.* += 1;

        var expect_value = true;
        while (true) {
            skipWhitespace(input, index, line, line_start);

            if (index.* >= input.len) {
                return error.SyntaxError;
            }
            const current_char = input[index.*];

            switch (current_char) {
                '{' => {
                    const nested_array = try parseArray(input, spool, src_name, index, line, line_start, allocator);
                    try array.push(Value{ .data = .{ .array = nested_array } });
                    expect_value = false;
                },
                '#' => {
                    std.log.warn("[{s}] Error at line {}, col {}: Directives not implemented", .{ src_name, line.*, index.* - line_start.* });
                    return error.NotImplemented;
                },
                '@' => {
                    var foundQuote = false;
                    const word = try getWord(input, src_name, index, line, line_start, &[_]u8{ ',', ';', '}' }, &foundQuote, allocator);
                    allocator.free(word);
                    std.log.warn("[{s}] Error at line {}, col {}: Expressions not implemented", .{ src_name, line.*, index.* - line_start.* });
                    return error.NotImplemented;
                },
                '}' => {
                    if (expect_value) {
                        if (index.* > 0 and input[index.* - 1] != '{' and input[index.* - 1] != ',' and input[index.* - 1] != ';') {
                            var temp_index = index.*;
                            while (temp_index > 0 and std.ascii.isWhitespace(input[temp_index - 1])) temp_index -= 1;
                            var value_start = temp_index;
                            while (value_start > 0 and input[value_start - 1] != '{' and input[value_start - 1] != ',' and input[value_start - 1] != ';' and !std.ascii.isWhitespace(input[value_start - 1])) value_start -= 1;
                            const found = input[value_start..temp_index];
                            if (found.len > 0) {
                                if (found[0] == '@') {
                                    std.log.warn("[{s}] Error at line {}, col {}: Expressions not implemented", .{ src_name, line.*, value_start - line_start.* });
                                    return error.NotImplemented;
                                }
                                if (found.len >= 6 and std.mem.eql(u8, found[0..6], "__EVAL")) {
                                    std.log.warn("[{s}] Error at line {}, col {}: Evaluate not yet implemented", .{ src_name, line.*, value_start - line_start.* });
                                    return error.NotImplemented;
                                }
                                var value = try scanInt(found, allocator) orelse
                                    try scanFloat(found, allocator) orelse
                                    try genValue(found, spool, allocator);
                                errdefer value.deinit(allocator);
                                try array.push(value);
                            }
                        }
                    }
                    index.* += 1;
                    return array;
                },
                ',' => {
                    index.* += 1;
                    expect_value = true;
                },
                ';' => {
                    std.log.warn("[{s}] Warning at line {}, col {}: Using ';' as array separator is deprecated, use ',' instead.", .{ src_name, line.*, index.* - line_start.* });
                    index.* += 1;
                    expect_value = true;
                },
                else => {
                    var foundQuote = false;
                    var found: []const u8 = &[_]u8{};
                    if (input[index.*] == '"') {
                        found = try getWord(input, src_name, index, line, line_start, &[_]u8{ ',', ';', '}' }, &foundQuote, allocator);
                    } else {
                        found = getUnquotedSlice(input, index, line, line_start, &[_]u8{ ',', ';', '}' });
                        foundQuote = false;
                    }

                    if (found.len > 0) {
                        if (found[0] == '@') {
                            std.log.warn("[{s}] Error at line {}, col {}: Expressions not implemented", .{ src_name, line.*, index.* - line_start.* });
                            return error.NotImplemented;
                        }
                        if (found.len >= 6 and std.mem.eql(u8, found[0..6], "__EVAL")) {
                            std.log.warn("[{s}] Error at line {}, col {}: Evaluate not yet implemented", .{ src_name, line.*, index.* - line_start.* });
                            return error.NotImplemented;
                        }
                        var value = try scanInt(found, allocator) orelse
                            try scanFloat(found, allocator) orelse
                            try genValue(found, spool, allocator);
                        errdefer value.deinit(allocator);
                        try array.push(value);
                        if (foundQuote) allocator.free(found);
                    }
                    expect_value = false;
                },
            }
        }

        return error.Unimplemented;
    }

    fn scanHex(val: []const u8) ?i32 {
        if (val.len < 3) return null;

        if (!std.ascii.eqlIgnoreCase(val[0..2], "0x")) return null;

        const hex_part = val[2..];
        if (hex_part.len == 0) return null;

        for (hex_part) |c| {
            if (!std.ascii.isHex(c)) return null;
        }

        return std.fmt.parseInt(i32, hex_part, 16) catch null;
    }

    fn scanInt(input: []const u8, allocator: Allocator) !?Value {
        if (input.len == 0) return null;

        if (scanIntPlain(input)) |val| {
            return try genValue(val, undefined, allocator);
        }

        if (scanHex(input)) |val| {
            return try genValue(val, undefined, allocator);
        }

        return null;
    }

    fn scanIntPlain(ptr: []const u8) ?i32 {
        if (ptr.len == 0) return null;

        return std.fmt.parseInt(i32, ptr, 10) catch null;
    }

    fn scanInt64Plain(ptr: []const u8) ?i64 {
        if (ptr.len == 0) return null;

        return std.fmt.parseInt(i64, ptr, 10) catch null;
    }

    fn scanInt64(input: []const u8, allocator: Allocator) !?Value {
        if (input.len == 0) return null;

        if (scanInt64Plain(input)) |val| {
            return try genValue(val, undefined, allocator);
        }

        if (scanHex(input)) |val| {
            return try genValue(val, undefined, allocator);
        }

        return null;
    }

    fn scanFloatPlain(ptr: []const u8) ?f32 {
        if (ptr.len == 0) return null;

        return std.fmt.parseFloat(f32, ptr) catch null;
    }

    fn scanDb(ptr: []const u8) ?f32 {
        if (ptr.len < 3 or ptr[0] != 'd' or ptr[1] != 'b') return null;

        const db_part = ptr[2..];

        const db_value = std.fmt.parseFloat(f32, db_part) catch {
            std.debug.print("invalid db value {s}\n", .{ptr});
            return null;
        };

        return std.math.pow(f32, 10.0, db_value * (1.0 / 20.0));
    }

    fn scanFloat(ptr: []const u8, allocator: Allocator) !?Value {
        if (ptr.len == 0) return null;

        if (scanFloatPlain(ptr)) |val| {
            return try genValue(val, undefined, allocator);
        }

        if (scanDb(ptr)) |val| {
            return try genValue(val, undefined, allocator);
        }

        return null;
    }

    fn getWord(input: []const u8, src_name: []const u8, index: *usize, line: *usize, line_start: *usize, terminators: []const u8, found_quote: *bool, allocator: Allocator) ![]const u8 {
        var result = std.ArrayList(u8).init(allocator);
        defer result.deinit();

        skipWhitespace(input, index, line, line_start);

        if (input[index.*] == '"') {
            index.* += 1;
            found_quote.* = true;
            while (index.* < input.len) {
                const c = input[index.*];
                if (c == '"') {
                    index.* += 1;
                    if (index.* < input.len and input[index.*] != '"') {
                        skipWhitespace(input, index, line, line_start);

                        if (index.* < input.len and input[index.*] != '\\') {
                            return try result.toOwnedSlice();
                        }
                        index.* += 1;
                        if (index.* < input.len and input[index.*] != 'n') {
                            std.log.warn("[{s}] Error at line {}, col {}: invalid escape sequence", .{ src_name, line.*, index.* - line_start.* });
                            return error.SyntaxError;
                        }
                        skipWhitespace(input, index, line, line_start);

                        if (index.* < input.len and input[index.*] != '"') {
                            std.log.warn("[{s}] Error at line {}, col {}: expected '\"' after escape sequence", .{ src_name, line.*, index.* - line_start.* });
                            return error.SyntaxError;
                        }

                        index.* += 1;
                        try result.append('\n');
                    } else {
                        index.* += 1;
                        try result.append('"');
                    }
                } else {
                    if (c == '\n' or c == '\r') {
                        std.log.warn("[{s}] Error at line {}, col {}: End of line encountered", .{ src_name, line.*, index.* - line_start.* });
                        return error.SyntaxError;
                    }
                    try result.append(c);
                    index.* += 1;
                    continue;
                }
            }
            std.log.warn("[{s}] Error at line {}, col {}: unterminated string literal", .{ src_name, line.*, index.* - line_start.* });
            return error.SyntaxError;
        } else {
            found_quote.* = false;
            var c = input[index.*];
            while (index.* < input.len and std.mem.indexOfScalar(u8, terminators, c) == null) {
                if (c == '\n' or c == '\r') {
                    while (true) {
                        skipWhitespace(input, index, line, line_start);
                        if (input[index.*] != '#') {
                            break;
                        }
                        std.log.warn("[{s}] Error at line {}, col {}: Directives not implemented", .{ src_name, line.*, index.* - line_start.* });
                        return error.NotImplemented;
                    }
                    c = input[index.*];
                    if (std.mem.indexOfScalar(u8, terminators, c) == null) {
                        std.log.warn("[{s}] Error at line {}, col {}: Expected unquoted terminator got '{}'", .{ src_name, line.*, index.* - line_start.*, c });
                    }
                } else {
                    index.* += 1;
                    if (index.* >= input.len) break;
                    try result.append(c);
                    c = input[index.*];
                }
            }

            return try result.toOwnedSlice();
        }
    }

    fn skipWhitespace(input: []const u8, index: *usize, line: *usize, line_start: *usize) void {
        while (index.* < input.len) {
            const c = input[index.*];
            if (c == '\n') {
                line.* += 1;
                index.* += 1;
                line_start.* = index.*;
            } else if (std.ascii.isWhitespace(c)) {
                index.* += 1;
            } else {
                break;
            }
        }
    }

    fn getAlphaWord(input: []const u8, index: *usize, line: *usize, line_start: *usize) []const u8 {
        skipWhitespace(input, index, line, line_start);

        const word_start = index.*;

        while (index.* < input.len) {
            const c = input[index.*];
            if (!(std.ascii.isAlphanumeric(c) or c == '_')) {
                break;
            }
            index.* += 1;
        }

        return input[word_start..index.*];
    }
};
