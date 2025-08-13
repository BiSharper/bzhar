const std = @import("std");

const Self = @This();

const N: i32 = 0x1000;
const FILL: u8 = 0x20;
const F: i32 = 0x12;
const MATCH_THRESHOLD: u8 = 0x2;
const BUF_SIZE: i32 = N + F - 1;

pub fn encode(allocator: std.mem.Allocator, input: []const u8, signed_checksum: bool) ![]u8 {
    var context = Self.init();
    const input_len: i32 = if (input.len > std.math.maxInt(i32))
        return error.BufferTooLong
    else
        @intCast(input.len);

    const max_out: i32 = @intCast(@max(std.math.maxInt(i32), input_len + (@divTrunc(input_len, 8)) + 8));
    var out = try allocator.alloc(u8, @intCast(max_out));
    errdefer allocator.free(out);

    var out_idx: i32 = 0;
    var in_idx: i32 = 0;
    var text_size: i32 = 0;
    var codesize: i32 = 0;
    var csum: i32 = 0;
    var last_match_len: i32 = 0;
    var cbuf = [_]u8{0} ** 17;
    var cbuf_idx: u5 = 1;
    var mask: u8 = 1;
    var s: i32 = 0;
    var r: i32 = N - F;
    var c: u8 = undefined;

    var len: i32 = 0;
    while (len < F and in_idx < input_len) : (len += 1) {
        c = input[@intCast(in_idx)];
        context.text_buf[@intCast(r + len)] = c;

        in_idx += 1;
        csum = incrementChecksum(csum, c, signed_checksum);
    }
    text_size = len;

    std.debug.assert(text_size > 0);
    var i: i32 = 1;
    while (i <= F) : (i += 1) {
        context.insertNode(r - i);
    }
    context.insertNode(r);

    while (true) {
        if (context.match_len > len) context.match_len = len;

        if (context.match_len <= MATCH_THRESHOLD) {
            context.match_len = 1;
            cbuf[0] |= mask;
            cbuf[@intCast(cbuf_idx)] = context.text_buf[@intCast(r)];
            cbuf_idx += 1;
        } else {
            const mp: u8 = @intCast((r - context.match_pos) & (N - 1));
            cbuf[cbuf_idx] = mp;
            cbuf_idx += 1;
            cbuf[cbuf_idx] = @intCast(((mp >> 4) & 0xF0) | (context.match_len - (MATCH_THRESHOLD + 1)));
            cbuf_idx += 1;
        }

        mask <<= 1;
        if (mask == 0) {
            @memcpy(out[@intCast(out_idx)..@intCast(out_idx + cbuf_idx)], cbuf[0..@intCast(cbuf_idx)]);
            codesize += cbuf_idx;
            out_idx += cbuf_idx;
            cbuf[0] = 0;
            cbuf_idx = 1;
            mask = 1;
        }

        last_match_len = context.match_len;

        i = 0;
        while (i < last_match_len and in_idx < input_len) : (i += 1) {
            context.deleteNode(s);

            c = input[@intCast(in_idx)];
            in_idx += 1;

            context.text_buf[@intCast(s)] = c;
            csum = incrementChecksum(csum, c, signed_checksum);

            if (s < F - 1) context.text_buf[@intCast(s + N)] = c;
            s += 1;
            s &= N - 1;
            r += 1;
            r &= N - 1;
            context.insertNode(r);
        }

        text_size += 1;
        while (i < last_match_len) : (i += 1) {
            context.deleteNode(s);
            s = (s + 1) & (N - 1);
            r = (r + 1) & (N - 1);
            len -= 1;
            if (len > 0) context.insertNode(r);
        }

        if (len <= 0) break;
    }
    if (cbuf_idx > 1) {
        @memcpy(out[@intCast(out_idx)..@intCast(out_idx + cbuf_idx)], cbuf[0..@intCast(cbuf_idx)]);
        codesize += cbuf_idx;
        out_idx += cbuf_idx;
    }

    @memcpy(out[@intCast(out_idx)..@intCast(out_idx + @sizeOf(i32))], @as([*]const u8, @ptrCast(&csum))[0..@sizeOf(i32)]);
    out_idx += @sizeOf(i32);

    return try allocator.realloc(out, @intCast(out_idx));
}

pub fn decode(allocator: std.mem.Allocator, input: []const u8, expected_len: usize, signed_checksum: bool) ![]u8 {
    if (input.len == 0 or expected_len == 0) {
        return try allocator.alloc(u8, 0);
    }
    if (expected_len >= std.math.maxInt(i32) or input.len >= std.math.maxInt(i32)) {
        return error.DataToLarge;
    }

    var output = try allocator.alloc(u8, expected_len);
    errdefer allocator.free(output);

    var out_idx: i32 = 0;
    var in_idx: i32 = 0;
    var bytes_left: i32 = @intCast(expected_len);
    var text_buf = [_]u8{FILL} ** BUF_SIZE;
    var csum: i32 = 0;

    var r: i32 = N - F;
    var flags: i32 = 0;

    while (bytes_left > 0) {
        var c: u8 = 0;

        flags >>= 1;
        if ((flags & 256) == 0) {
            c = input[@intCast(in_idx)];
            in_idx += 1;
            flags = @as(i32, c) | 0xff00;
        }

        try boundsCheck(input.len, in_idx);

        if ((flags & 1) != 0) {
            c = input[@intCast(in_idx)];
            in_idx += 1;

            try boundsCheck(input.len, in_idx);
            csum = incrementChecksum(csum, c, signed_checksum);

            output[@intCast(out_idx)] = c;
            out_idx += 1;
            bytes_left -= 1;

            text_buf[@intCast(r)] = c;

            r += 1;
            r &= N - 1;
            continue;
        }

        var i: i32 = @intCast(input[@intCast(in_idx)]);
        in_idx += 1;
        var j: i32 = @intCast(input[@intCast(in_idx)]);
        in_idx += 1;
        try boundsCheck(input.len, in_idx);

        i |= (j & 0xf0) << 4;
        j &= 0x0f;
        j += MATCH_THRESHOLD;

        if ((j + 1) > bytes_left) {
            std.log.debug("LZSS overflow", .{});
            return error.LZSSOverflow;
        }

        i = @intCast(r - i);
        j += i;

        while (i <= j) : (i += 1) {
            c = text_buf[@intCast(i & (N - 1))];
            csum = incrementChecksum(csum, c, signed_checksum);

            output[@intCast(out_idx)] = c;
            out_idx += 1;
            bytes_left -= 1;

            text_buf[@intCast(r)] = c;
            r += 1;
            r &= N - 1;
        }
    }

    if (in_idx + 4 != input.len) return error.ExtraData;

    const csr = std.mem.readInt(i32, input[@intCast(in_idx)..][0..4], .little);

    if (csr != csum) {
        return error.ChecksumMismatch;
    }

    return output;
}

pub fn random(allocator: std.mem.Allocator, rng: std.Random, expected_output_size: usize, signed_checksum: bool) ![]u8 {
    const MIN_MATCH: i32 = MATCH_THRESHOLD + 1;
    const MAX_MATCH: i32 = F;
    const MATCH_PROB: f32 = 0.3; //Directly correlates to entropy
    //very liberal with size here we could probably get a lower higher bound
    const max_size = if (expected_output_size == 0) 4 else expected_output_size * 2 + 8;
    var buffer = try allocator.alloc(u8, if (expected_output_size == 0) 4 else max_size);
    errdefer allocator.free(buffer);

    if (expected_output_size == 0) {
        std.mem.writeInt(u32, buffer[0..4], 0, .little);
        return buffer;
    }
    var text_buf: [N]u8 = .{FILL} ** N;
    var r: usize = N - F;
    var decomp: usize = 0;
    var csum: i32 = 0;
    var idx: usize = 0;
    while (decomp < expected_output_size) {
        const flag_idx = idx;
        idx += 1;
        var flag: u8 = 0;
        var ops_in_this_block: u4 = 0;
        for (0..8) |op_idx_in_block| {
            if (decomp >= expected_output_size) {
                break;
            }
            const remaining = expected_output_size - decomp;
            const match = (remaining >= MIN_MATCH and rng.float(f32) < MATCH_PROB);

            decomp += blk: {
                if (!match) {
                    const next = rng.int(u8);
                    flag |= (@as(u8, 1) << @intCast(op_idx_in_block));

                    buffer[idx] = next;
                    idx += 1;
                    text_buf[r] = next;
                    r = (r + 1) & (N - 1);
                    csum = incrementChecksum(csum, next, signed_checksum);

                    break :blk 1;
                } else {
                    const out_len = rng.intRangeAtMost(
                        usize,
                        MIN_MATCH,
                        @min(MAX_MATCH, remaining),
                    );

                    const length_code: u4 = @intCast(out_len - MIN_MATCH);
                    std.debug.assert(length_code <= (MAX_MATCH - MIN_MATCH) and length_code <= 0x0F);

                    const source_abs: u12 = @truncate(rng.intRangeAtMost(usize, 0, N - 1));
                    const offset_val: u12 = @truncate((r -% source_abs) & (N - 1));

                    buffer[idx] = @truncate(offset_val);
                    idx += 1;
                    buffer[idx] = (@as(u8, @truncate(offset_val >> 8)) << 4) | length_code;
                    idx += 1;

                    for (0..out_len) |i| {
                        const c = text_buf[(source_abs + i) & (N - 1)];

                        csum = incrementChecksum(csum, c, signed_checksum);
                        text_buf[r] = c;
                        r = (r + 1) & (N - 1);
                    }

                    break :blk out_len;
                }
            };
            ops_in_this_block += 1;
        }

        if (ops_in_this_block == 0) {
            idx = flag_idx;
        } else {
            buffer[flag_idx] = flag;
        }
    }
    std.mem.writeInt(i32, buffer[idx .. idx + 4][0..4], csum, .little);
    idx += 4;

    return try allocator.realloc(buffer, idx);
}

text_buf: [BUF_SIZE]u8,
left: [N + 1]i32,
right: [N + 257]i32,
parent: [N + 1]i32,
match_pos: i32,
match_len: i32,

fn init() Self {
    var context = Self{
        .text_buf = [_]u8{FILL} ** BUF_SIZE,
        .left = [_]i32{N} ** (N + 1),
        .right = [_]i32{N} ** (N + 257),
        .parent = [_]i32{N} ** (N + 1),
        .match_pos = 0,
        .match_len = 0,
    };

    var i: i32 = N + 1;
    while (i <= N + 256) : (i += 1) {
        context.right[@intCast(i)] = N;
    }

    i = 0;
    while (i < N) : (i += 1) {
        context.parent[@intCast(i)] = N;
    }

    return context;
}

fn insertNode(self: *Self, r: i32) void {
    var i: i32 = undefined;
    var cmp: bool = true;
    var p: i32 = N + 1 + self.text_buf[@intCast(r)];

    self.right[@intCast(r)] = N;
    self.left[@intCast(r)] = N;
    self.match_len = 0;

    while (true) {
        if (cmp) {
            if (self.right[@intCast(p)] != N) {
                p = self.right[@intCast(p)];
            } else {
                self.right[@intCast(p)] = r;
                self.parent[@intCast(r)] = p;
                return;
            }
        } else {
            if (self.left[@intCast(p)] != N) {
                p = self.left[@intCast(p)];
            } else {
                self.left[@intCast(p)] = r;
                self.parent[@intCast(r)] = p;
                return;
            }
        }

        const tbp = self.text_buf[@intCast(p + 1)..];
        const kp = self.text_buf[@intCast(r + 1)..];

        i = 1;
        while (i < F) : (i += 1) {
            if (kp[@intCast(i - 1)] != tbp[@intCast(i - 1)]) {
                cmp = kp[@intCast(i - 1)] >= tbp[@intCast(i - 1)];
                break;
            }
        }

        if (i > self.match_len) {
            self.match_pos = p;
            self.match_len = i;
            if (self.match_len >= F) break;
        }
    }

    self.parent[@intCast(r)] = self.parent[@intCast(p)];
    self.left[@intCast(r)] = self.left[@intCast(p)];
    self.right[@intCast(r)] = self.left[@intCast(p)];

    self.parent[@intCast(self.left[@intCast(p)])] = r;
    self.parent[@intCast(self.right[@intCast(p)])] = r;

    if (self.right[@intCast(self.parent[@intCast(p)])] == p) {
        self.right[@intCast(self.parent[@intCast(p)])] = r;
    } else {
        self.left[@intCast(self.parent[@intCast(p)])] = r;
    }

    self.parent[@intCast(p)] = N;
}

fn deleteNode(self: *Self, p: i32) void {
    var q: i32 = undefined;

    if (self.parent[@intCast(p)] == N) return;

    if (self.right[@intCast(p)] == N) {
        q = self.left[@intCast(p)];
    } else if (self.left[@intCast(p)] == N) {
        q = self.right[@intCast(p)];
    } else {
        q = self.left[@intCast(p)];
        if (self.right[@intCast(q)] != N) {
            while (self.right[@intCast(q)] != N) {
                q = self.right[@intCast(q)];
            }

            self.right[@intCast(self.parent[@intCast(q)])] = self.left[@intCast(q)];
            self.parent[@intCast(self.left[@intCast(q)])] = self.parent[@intCast(q)];
            self.left[@intCast(q)] = self.left[@intCast(p)];
            self.parent[@intCast(self.left[@intCast(p)])] = q;
        }
        self.right[@intCast(q)] = self.right[@intCast(p)];
        self.parent[@intCast(self.right[@intCast(p)])] = q;
    }

    self.parent[@intCast(q)] = self.parent[@intCast(p)];
    if (self.right[@intCast(self.parent[@intCast(p)])] == p) {
        self.right[@intCast(self.parent[@intCast(p)])] = q;
    } else {
        self.left[@intCast(self.parent[@intCast(p)])] = q;
    }
    self.parent[@intCast(p)] = N;
}

inline fn incrementChecksum(csum: i32, increment: u8, signed_checksum: bool) i32 {
    return csum +% if (signed_checksum) @as(i32, increment) else @as(i32, @intCast(@as(u32, increment)));
}

inline fn boundsCheck(len: usize, idx: i32) !void {
    if (idx > len) {
        std.debug.print("LZSS failed to read stream", .{});
        return error.InputTooShort;
    }
}

const testing = std.testing;
const test_allocator = testing.allocator;

test "Generate and decompress random LZSS" {
    var prng = std.Random.DefaultPrng.init(5);
    const rng = prng.random();

    const sizes = [_]usize{ 10, 50, 100, 1024, 10000, 100000, 1000000, 5000000, 10000000 };

    for (sizes) |size| {
        const compressed = try random(
            test_allocator,
            rng,
            size,
            false,
        );
        defer test_allocator.free(compressed);

        const decompressed = decode(
            test_allocator,
            compressed,
            size,
            false,
        ) catch |err| switch (err) {
            error.ChecksumMismatch => {
                continue;
            },
            else => return err,
        };

        defer test_allocator.free(decompressed);
    }
}

test "LZSS De/Compress Roundtrip" {
    const test_cases = [_][]const u8{
        "a",
        "Hello World",
        "Bisharper.... again.... in another languague? I think you've finally lost it ellie; atleast your learning.",
        "AAAAAAAAAAAAAAAAAAAAAAAAAA", //Repeted patterns
        "101010101010101010101010101010",
        "This is a test. This is a test. This is a test.",
        "The quick brown fox jumps over the lazy dog",
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent posuere eros a cursus tincidunt.",
        "Эй, жлоб! Где туз? Прячь юных съёмщиц в шкаф.", //unicode
        &[_]u8{ 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5 },
    };

    for (test_cases) |input| {
        const compressed = try encode(test_allocator, input, false);

        defer test_allocator.free(compressed);

        const decompressed = try decode(test_allocator, compressed, input.len, false);

        defer test_allocator.free(decompressed);

        try testing.expectEqualSlices(u8, input, decompressed);
    }
}

test "LZSS with signed checksum" {
    const test_data = "Test with signed checksum";

    const compressed = try encode(test_allocator, test_data, true);
    defer test_allocator.free(compressed);

    const decompressed = try decode(test_allocator, compressed, test_data.len, true);
    defer test_allocator.free(decompressed);

    try testing.expectEqualSlices(u8, test_data, decompressed);
}

test "LZSS with unsigned checksum" {
    const test_data = "Test with signed checksum";

    const compressed = try encode(test_allocator, test_data, true);
    defer test_allocator.free(compressed);

    const decompressed = try decode(test_allocator, compressed, test_data.len, true);
    defer test_allocator.free(decompressed);

    try testing.expectEqualSlices(u8, test_data, decompressed);
}
