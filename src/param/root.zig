const std = @import("std");

pub const src = @import("src.zig");
pub const values = @import("value.zig");
pub const context = @import("context.zig");
pub const Value = values.Value;
pub const Array = values.Array;
pub const Root = context.Root;
pub const Context = context.Context;
pub const Parameter = context.Parameter;
pub const Access = context.Access;

const testing = std.testing;

const Source = src.Source;

test "[param] getValueByPath supports nested classes and arrays" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    var inner1 = Array.init(testing.allocator);
    try inner1.push(Value{ .data = .{ .i32 = 1 } });
    try inner1.push(Value{ .data = .{ .i32 = 2 } });

    var inner2 = Array.init(testing.allocator);
    try inner2.push(Value{ .data = .{ .i32 = 3 } });
    try inner2.push(Value{ .data = .{ .i32 = 4 } });
    try inner2.push(Value{ .data = .{ .i32 = 5 } });

    var inner3b = Array.init(testing.allocator);
    try inner3b.push(Value{ .data = .{ .i32 = 7 } });
    try inner3b.push(Value{ .data = .{ .i32 = 8 } });
    try inner3b.push(Value{ .data = .{ .i32 = 9 } });

    var inner3 = Array.init(testing.allocator);
    try inner3.push(Value{ .data = .{ .i32 = 6 } });
    try inner3.push(Value{ .data = .{ .array = inner3b } });

    var outer = Array.init(testing.allocator);
    try outer.push(Value{ .data = .{ .array = inner1 } });
    try outer.push(Value{ .data = .{ .array = inner2 } });
    try outer.push(Value{ .data = .{ .array = inner3 } });

    var A = try root_ctx.createClass("A", null);
    defer A.release();
    var B = try A.createClass("B", null);
    defer B.release();
    try B.createValue("arr", outer, false);

    const v_opt = root.getValueByPath("config.A.B.arr[2][1][2]");
    try testing.expect(v_opt != null);
    const v = v_opt.?.*;
    try testing.expect(v.data == .i32);
    try testing.expect(v.getNumber() == 9);
}

test "[param] create/remove classes and derivative guard" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    var base = try root_ctx.createClass("Base", null);
    defer base.release();

    var child = try root_ctx.createClass("Child", base);

    try testing.expectError(error.ClassHasDerivatives, root_ctx.removeClass("Base"));
    try root_ctx.removeClass("Child");
    child.release();
    try root_ctx.removeClass("Base");
}

test "[param] creating duplicate class returns existing (no error)" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    var a = try root_ctx.createClass("Dup", null);
    defer a.release();

    var b = try root_ctx.createClass("Dup", null);
    defer b.release();

    try testing.expect(a == b);
}

test "[param] removeClass on non-existent class returns ClassNotFound" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    try testing.expectError(error.ClassNotFound, root_ctx.removeClass("DoesNotExist"));
}

test "[param] extend circular dependency is rejected" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    var A = try root_ctx.createClass("A", null);
    defer A.release();
    var B = try root_ctx.createClass("B", A);
    defer B.release();

    try testing.expectError(error.CircularDependency, A.extend(B));
}

test "[param] Context.getPath and nested classes" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    var A = try root_ctx.createClass("A", null);
    defer A.release();
    var B = try A.createClass("B", null);
    defer B.release();
    var C = try B.createClass("C", null);
    defer C.release();

    const path = try C.getPath(testing.allocator);
    defer testing.allocator.free(path);
    try testing.expectEqualStrings("config.a.b.c", path);
}

test "[param] Parameter.getPath for nested arrays" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    var inner1 = Array.init(testing.allocator);
    try inner1.push(Value{ .data = .{ .i32 = 11 } });
    try inner1.push(Value{ .data = .{ .i32 = 22 } });

    var inner2 = Array.init(testing.allocator);
    try inner2.push(Value{ .data = .{ .i32 = 33 } });

    var outer = Array.init(testing.allocator);
    try outer.push(Value{ .data = .{ .array = inner1 } });
    try outer.push(Value{ .data = .{ .array = inner2 } });

    try root_ctx.createValue("arr", outer, false);

    root_ctx.rw_lock.lockShared();
    defer root_ctx.rw_lock.unlockShared();
    const par_opt = root_ctx.params.get("arr");
    try testing.expect(par_opt != null);
    const par = par_opt.?;

    const target_value: *const Value = &par.value.data.array.values.items[1].data.array.values.items[0];
    const pth_opt = try par.getPath(target_value, testing.allocator);
    try testing.expect(pth_opt != null);
    const pth = pth_opt.?;
    defer testing.allocator.free(pth);
    try testing.expectEqualStrings("arr[1][0]", pth);
}

test "[param] Value.toSyntax and Array.toSyntax" {
    const s = try testing.allocator.dupe(u8, "He said \"hi\"\n");
    var v = Value{ .data = .{ .string = s } };
    const vs = try v.toSyntax(testing.allocator);
    defer testing.allocator.free(vs);
    testing.allocator.free(s);
    try testing.expect(vs.len >= 2);
    try testing.expect(vs[0] == '"');
    try testing.expect(vs[vs.len - 1] == '"');

    var arr = Array.init(testing.allocator);
    try arr.push(Value{ .data = .{ .i32 = 1 } });
    try arr.push(Value{ .data = .{ .i32 = 2 } });
    const as = try arr.toSyntax(testing.allocator);
    defer testing.allocator.free(as);
    try testing.expect(as.len >= 2);
    try testing.expect(as[0] == '{');
    try testing.expect(as[as.len - 1] == '}');
    arr.deinit(testing.allocator);
}

test "[param] Array.clone deep copy" {
    var inner = Array.init(testing.allocator);
    try inner.push(Value{ .data = .{ .i32 = 2 } });

    var original = Array.init(testing.allocator);
    try original.push(Value{ .data = .{ .i32 = 1 } });
    try original.push(Value{ .data = .{ .array = inner } });

    const syn_orig = try original.toSyntax(testing.allocator);
    defer testing.allocator.free(syn_orig);

    var cloned = try original.clone(testing.allocator);
    defer cloned.deinit(testing.allocator);

    const syn_clone = try cloned.toSyntax(testing.allocator);
    defer testing.allocator.free(syn_clone);

    try testing.expectEqualStrings(syn_orig, syn_clone);

    original.deinit(testing.allocator);
}

test "[param] Context.toSyntax contains class and params" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    var cx = try root_ctx.createClass("ForSyntax", null);
    defer cx.release();

    try cx.createValue("foo", 2, false);
    const txt = try cx.toSyntax(testing.allocator, 0);
    defer testing.allocator.free(txt);
    try testing.expect(std.mem.indexOf(u8, txt, "class forsyntax") != null);
    try testing.expect(std.mem.indexOf(u8, txt, "foo = 2;") != null);
}

test "[param] Access.toSyntax" {
    const s = try Access.ReadOnly.toSyntax(testing.allocator);
    defer testing.allocator.free(s);
    try testing.expectEqualStrings("access = 2;\n", s);
}

fn testDataPath(allocator: std.mem.Allocator, rel: []const u8) ![]u8 {
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);
    return try std.fs.path.join(allocator, &[_][]const u8{ cwd, "testdata", "param", rel });
}

test "[param] parsing creates classes and params in root context" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    const input = "class A; foo = 42;";

    var source_inst = try Source.memory("mem", input, testing.allocator);
    defer source_inst.deinit();
    try root_ctx.parse(source_inst, false);

    root_ctx.rw_lock.lockShared();
    defer root_ctx.rw_lock.unlockShared();

    try testing.expect(root_ctx.children.get("a") != null);
    const par_opt = root_ctx.params.get("foo");
    try testing.expect(par_opt != null);
    try testing.expect(par_opt.?.value.getNumber() == 42);
}

test "[param] parsing nested classes and parameters" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    const input =
        "class A {\n" ++
        "    class B { c = 5; };\n" ++
        "    d = \"x\";\n" ++
        "};";

    var source_inst = try Source.memory("mem", input, testing.allocator);
    defer source_inst.deinit();
    try root_ctx.parse(source_inst, false);

    root_ctx.rw_lock.lockShared();
    defer root_ctx.rw_lock.unlockShared();

    const A_opt = root_ctx.children.get("a");
    try testing.expect(A_opt != null);
    const A = A_opt.?;

    const d_opt = A.params.get("d");
    try testing.expect(d_opt != null);

    const B_opt = A.children.get("b");
    try testing.expect(B_opt != null);
    const B = B_opt.?;

    const c_opt = B.params.get("c");
    try testing.expect(c_opt != null);
    try testing.expect(c_opt.?.value.getNumber() == 5);
}

test "[param] parsing array assign and add" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    {
        var src1 = try Source.memory("mem1", "arr[] = {1,2};", testing.allocator);
        defer src1.deinit();
        try root_ctx.parse(src1, false);
    }

    {
        var src2 = try Source.memory("mem2", "arr[] += {3};", testing.allocator);
        defer src2.deinit();
        try root_ctx.parse(src2, false);
    }

    {
        root_ctx.rw_lock.lockShared();
        defer root_ctx.rw_lock.unlockShared();
        const par_opt = root_ctx.params.get("arr");
        try testing.expect(par_opt != null);
        const par = par_opt.?;
        try testing.expect(par.value.data == .array);
        try testing.expect(par.value.data.array.values.items.len == 3);
        try testing.expect(par.value.data.array.values.items[0].getNumber() == 1);
        try testing.expect(par.value.data.array.values.items[1].getNumber() == 2);
        try testing.expect(par.value.data.array.values.items[2].getNumber() == 3);
    }

    {
        var src3 = try Source.memory("mem3", "arr[] = {7};", testing.allocator);
        defer src3.deinit();
        try root_ctx.parse(src3, false);
    }

    {
        root_ctx.rw_lock.lockShared();
        defer root_ctx.rw_lock.unlockShared();
        const par_opt = root_ctx.params.get("arr");
        try testing.expect(par_opt != null);
        const par = par_opt.?;
        try testing.expect(par.value.data == .array);
        try testing.expect(par.value.data.array.values.items.len == 1);
        try testing.expect(par.value.data.array.values.items[0].getNumber() == 7);
    }
}

test "[param] parsing delete removes class" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    var source_inst = try Source.memory("mem-del", "class X; delete X;", testing.allocator);
    defer source_inst.deinit();
    try root_ctx.parse(source_inst, false);

    root_ctx.rw_lock.lockShared();
    defer root_ctx.rw_lock.unlockShared();
    try testing.expect(root_ctx.children.get("x") == null);
}

test "[param] deletions tracked by source" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    // Create two classes, each from a different source
    var s1 = try Source.memory("s1", "class A;", testing.allocator);
    defer s1.deinit();
    try root_ctx.parse(s1, true);

    var s2 = try Source.memory("s2", "class B;", testing.allocator);
    defer s2.deinit();
    try root_ctx.parse(s2, true);

    // Now delete A from s2; provenance should record s2 for deletion of A
    var s3 = try Source.memory("s3", "delete A;", testing.allocator);
    defer s3.deinit();
    try root_ctx.parse(s3, true);

    root_ctx.rw_lock.lockShared();
    defer root_ctx.rw_lock.unlockShared();
    const del_src_opt = root_ctx.deletions.get("a");
    try testing.expect(del_src_opt != null);
    try testing.expectEqualStrings("s3", del_src_opt.?.name);

    // Iterate deletions via public iterator
    var it = root_ctx.deletionsIterator();
    defer it.deinit();
    const first = it.next();
    try testing.expect(first != null);
    try testing.expectEqualStrings("a", first.?.name);
    try testing.expectEqualStrings("s3", first.?.source.name);
}

test "[param] parsing reports syntax error" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    var source_inst = try Source.memory("mem-bad", "class Bad { a = 1", testing.allocator);
    defer source_inst.deinit();
    const root_ctx: *Context = root.retain();
    defer root_ctx.release();
    try testing.expectError(error.SyntaxError, root_ctx.parse(source_inst, false));
}

test "[param] parse from file: simple.cpp" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    const path = try testDataPath(testing.allocator, "simple.cpp");
    defer testing.allocator.free(path);

    var source_inst = try Source.file(path, testing.allocator);
    defer source_inst.deinit();

    try root_ctx.parse(source_inst, false);

    root_ctx.rw_lock.lockShared();
    defer root_ctx.rw_lock.unlockShared();
    try testing.expect(root_ctx.children.get("a") != null);
    const par_opt = root_ctx.params.get("foo");
    try testing.expect(par_opt != null);
    try testing.expect(par_opt.?.value.getNumber() == 42);
}

test "[param] parse from file: nested.cpp" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    const path = try testDataPath(testing.allocator, "nested.cpp");
    defer testing.allocator.free(path);

    var source_inst = try Source.file(path, testing.allocator);
    defer source_inst.deinit();

    try root_ctx.parse(source_inst, false);

    root_ctx.rw_lock.lockShared();
    defer root_ctx.rw_lock.unlockShared();
    const A_opt = root_ctx.children.get("a");
    try testing.expect(A_opt != null);
    const A = A_opt.?;
    const d_opt = A.params.get("d");
    try testing.expect(d_opt != null);
    const B_opt = A.children.get("b");
    try testing.expect(B_opt != null);
    const B = B_opt.?;
    const c_opt = B.params.get("c");
    try testing.expect(c_opt != null);
    try testing.expect(c_opt.?.value.getNumber() == 5);
}

test "[param] parse from file: game.cpp" {
    const root: *Root = try Root.create("config", testing.allocator);
    defer root.release();

    const root_ctx: *Context = root.retain();
    defer root_ctx.release();

    const path = try testDataPath(testing.allocator, "game.cpp");
    defer testing.allocator.free(path);

    var source_inst = try Source.file(path, testing.allocator);
    defer source_inst.deinit();

    try root_ctx.parse(source_inst, false);

    root_ctx.rw_lock.lockShared();
    defer root_ctx.rw_lock.unlockShared();
    const total = root_ctx.children.count() + root_ctx.params.count();
    try testing.expect(total > 0);
}

test "[param] benchmark parse: game.cpp" {
    const root_alloc = testing.allocator;

    const path = try testDataPath(testing.allocator, "game.cpp");
    defer root_alloc.free(path);

    const iterations: usize = 3;
    var totals_ns: u128 = 0;
    var min_ns: u128 = std.math.maxInt(u128);

    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        const root: *Root = try Root.create("config", root_alloc);
        defer root.release();

        const root_ctx: *Context = root.retain();
        defer root_ctx.release();

        var source_inst = try Source.file(path, root_alloc);
        defer source_inst.deinit();

        var timer = try std.time.Timer.start();
        try root_ctx.parse(source_inst, false);
        const elapsed_ns: u128 = timer.read();

        totals_ns += elapsed_ns;
        if (elapsed_ns < min_ns) min_ns = elapsed_ns;

        std.debug.print("[param] parse game.cpp iter {}: {d} ms\n", .{ i, @as(u64, @intCast(elapsed_ns / 1_000_000)) });
    }

    const avg_ns: u128 = totals_ns / iterations;
    std.debug.print("[param] parse game.cpp: min {d} ms, avg {d} ms\n", .{
        @as(u64, @intCast(min_ns / 1_000_000)),
        @as(u64, @intCast(avg_ns / 1_000_000)),
    });
}
