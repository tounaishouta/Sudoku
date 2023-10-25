const std = @import("std");
const assert = std.debug.assert;

pub fn main() !void {
    init();
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [256]u8 = undefined;
    var sudoku: Sudoku = undefined;
    while (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const problem = std.mem.trimRight(u8, line, "\r");
        const answer = sudoku.solve(problem, &buffer);
        try stdout.print("{s}\n", .{answer});
    }
}

const Sudoku = struct {
    state: [n_coords]State,
    count: BinaryHeap(count, n_blocks),
    fn solve(self: *Sudoku, problem: []const u8, buffer: []u8) []const u8 {
        self.reset();
        if (self.load(problem) and self.search()) {
            return self.dump(buffer);
        } else {
            return "NO SOLUTION";
        }
    }
    fn reset(self: *Sudoku) void {
        for (0..n_coords) |c| {
            self.state[c] = .tbd;
        }
        self.count.reset(size);
    }
    fn load(self: *Sudoku, problem: []const u8) bool {
        for (0..size) |i|
            for (0..size) |j|
                if (i * size + j < problem.len)
                    for (0..size) |k|
                        if (problem[i * size + j] == digits[k])
                            if (!self.adopt(coordOf(i, j, k))) return false;
        return true;
    }
    fn search(self: *Sudoku) bool {
        switch (self.count.min()) {
            0 => {
                return false;
            },
            1 => {
                const b = self.count.argmin();
                for (coords_of[b]) |c|
                    if (self.state[c] == .tbd)
                        return self.adopt(c) and self.search();
                unreachable;
            },
            2...size => {
                const b = self.count.argmin();
                var branch: Sudoku = undefined;
                for (coords_of[b]) |c| {
                    if (self.state[c] == .tbd) {
                        branch = self.*;
                        if (!branch.adopt(c)) continue;
                        if (!branch.search()) continue;
                        self.* = branch;
                        return true;
                    }
                }
                return false;
            },
            marked => {
                return true;
            },
            else => {
                unreachable;
            },
        }
    }
    fn dump(self: *Sudoku, buffer: []u8) []const u8 {
        for (0..size) |i| {
            for (0..size) |j| {
                buffer[i * size + j] = '.';
                for (0..size) |k| {
                    if (self.state[coordOf(i, j, k)] == .true) {
                        buffer[i * size + j] = digits[k];
                    }
                }
            }
        }
        return buffer[0 .. size * size];
    }
    fn adopt(self: *Sudoku, c: coord) bool {
        if (self.state[c] == .true) return true;
        if (self.state[c] == .false) return false;
        assert(self.state[c] == .tbd);
        self.state[c] = .true;
        for (blocks_of[c]) |b|
            self.mark(b);
        return true;
    }
    fn mark(self: *Sudoku, b: block) void {
        assert(self.count.get(b) != marked);
        self.count.set(b, marked);
        for (coords_of[b]) |c|
            self.reject(c);
    }
    fn reject(self: *Sudoku, c: coord) void {
        if (self.state[c] == .true) return;
        if (self.state[c] == .false) return;
        assert(self.state[c] == .tbd);
        self.state[c] = .false;
        for (blocks_of[c]) |b|
            self.countdown(b);
    }
    fn countdown(self: *Sudoku, b: block) void {
        if (self.count.get(b) == marked) return;
        self.count.set(b, self.count.get(b) - 1);
    }
};

const view = 4;
const unit = 3;
const size = unit * unit;
const n_blocks = view * size * size;
const n_coords = size * size * size;
const digits = "123456789";

var blocks_of: [n_coords][view]block = undefined;
var coords_of: [n_blocks][size]coord = undefined;

const index = usize; // 0..size
const block = usize; // 0..n_blocks
const coord = usize; // 0..n_coords

fn init() void {
    var found: [n_blocks]usize = .{0} ** n_blocks;
    for (0..size) |i| {
        for (0..size) |j| {
            for (0..size) |k| {
                const c = coordOf(i, j, k);
                blocks_of[c] = blocksOf(i, j, k);
                for (blocks_of[c]) |b| {
                    coords_of[b][found[b]] = c;
                    found[b] += 1;
                }
            }
        }
    }
}

fn coordOf(i: index, j: index, k: index) coord {
    return (i * size + j) * size + k;
}

fn blocksOf(i: index, j: index, k: index) [view]block {
    const p = i / unit * unit + j / unit;
    return .{
        (0 * size + i) * size + j,
        (1 * size + i) * size + k,
        (2 * size + j) * size + k,
        (3 * size + p) * size + k,
    };
}

const State = enum { tbd, true, false };

const count = u8; // 0..size
const marked: count = std.math.maxInt(count);

fn BinaryHeap(comptime T: type, comptime len: usize) type {
    return struct {
        value: [len]T,
        index: [len]usize,
        order: [len]usize,
        const Self = @This();
        fn reset(self: *Self, init_value: T) void {
            for (0..len) |i| {
                self.value[i] = init_value;
                self.index[i] = i;
                self.order[i] = i;
            }
        }
        fn min(self: *Self) T {
            return self.value[0];
        }
        fn argmin(self: *Self) usize {
            return self.index[0];
        }
        fn get(self: *Self, i: usize) T {
            return self.value[self.order[i]];
        }
        fn set(self: *Self, i: usize, new_value: T) void {
            const o = self.order[i];
            const old_value = self.value[o];
            self.value[o] = new_value;
            if (new_value < old_value)
                self.downsort(o);
            if (new_value > old_value)
                self.upsort(o);
        }
        fn downsort(self: *Self, o: usize) void {
            if (o == 0) return;
            const p = (o - 1) / 2;
            if (self.value[p] <= self.value[o]) return;
            self.swap(p, o);
            self.downsort(p);
        }
        fn upsort(self: *Self, o: usize) void {
            const p = 2 * o + 1;
            const q = 2 * o + 2;
            const swap_p = p < len and self.value[p] < self.value[o];
            const swap_q = q < len and self.value[q] < self.value[o];
            if (swap_p and (!swap_q or self.value[p] <= self.value[q])) {
                self.swap(o, p);
                self.upsort(p);
            }
            if (swap_q) {
                self.swap(o, q);
                self.upsort(q);
            }
        }
        fn swap(self: *Self, o: usize, p: usize) void {
            std.mem.swap(T, &self.value[o], &self.value[p]);
            std.mem.swap(usize, &self.index[o], &self.index[p]);
            std.mem.swap(usize, &self.order[self.index[o]], &self.order[self.index[p]]);
        }
    };
}
