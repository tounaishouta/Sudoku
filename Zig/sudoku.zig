const std = @import("std");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var inbuf: [256]u8 = undefined;
    var outbuf: [size * size]u8 = undefined;
    while (stdin.readUntilDelimiter(&inbuf, '\n')) |line| {
        const problem = std.mem.trimRight(u8, line, &.{'\r'});
        const answer = Sudoku.solve(problem, &outbuf);
        try stdout.print("{s}\n", .{answer});
    } else |err| switch (err) {
        error.EndOfStream => return,
        else => |leftover_err| return leftover_err,
    }
}

const Sudoku = struct {
    state: [n_coords]State,
    count: Counter,
    fn solve(problem: []const u8, buffer: []u8) []const u8 {
        var sudoku = Sudoku.init();
        sudoku.load(problem) catch return "NO SOLUTION";
        sudoku.search() catch return "NO SOLUTION";
        return sudoku.dump(buffer);
    }
    fn init() Sudoku {
        return .{
            .state = .{.undefined} ** n_coords,
            .count = Counter.init(size),
        };
    }
    fn load(self: *Sudoku, problem: []const u8) SudokuError!void {
        for (problem, 0..) |ch, ij| {
            if (ij >= size * size) break;
            const i = ij / size;
            const j = ij % size;
            for (digits, 0..size) |d, k| {
                if (ch == d) try self.assign(coordOf(i, j, k));
            }
        }
    }
    fn search(self: *Sudoku) SudokuError!void {
        if (self.count.empty()) return;
        const b = self.count.argmin();
        switch (self.count.get(b)) {
            0 => return SudokuError.NoSolution,
            1 => {
                for (coords_of[b]) |c| {
                    if (self.state[c] == .undefined) {
                        try self.assign(c);
                    }
                }
                return self.search();
            },
            else => {
                for (coords_of[b]) |c| {
                    if (self.state[c] == .undefined) {
                        var other = self.*;
                        other.assign(c) catch continue;
                        other.search() catch continue;
                        self.* = other;
                        return;
                    }
                }
                return SudokuError.NoSolution;
            },
        }
    }
    fn assign(self: *Sudoku, c: coord) SudokuError!void {
        const c0 = c;
        switch (self.state[c0]) {
            .true => return,
            .false => return SudokuError.NoSolution,
            .undefined => {
                self.state[c0] = .true;
                for (blocks_of[c0]) |b1| {
                    self.count.remove(b1);
                    for (coords_of[b1]) |c2| {
                        if (c2 == c0) continue;
                        switch (self.state[c2]) {
                            .true => unreachable,
                            .false => {},
                            .undefined => {
                                self.state[c2] = .false;
                                for (blocks_of[c2]) |b3| {
                                    if (b3 == b1) continue;
                                    self.count.decrement(b3);
                                }
                            },
                        }
                    }
                }
            },
        }
    }
    fn dump(self: *Sudoku, buffer: []u8) []u8 {
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
};

const SudokuError = error{NoSolution};

const block = usize;
const coord = usize;

const unit = 3;
const size = unit * unit;
const n_blocks = 4 * size * size;
const n_coords = size * size * size;
const digits = "123456789";

fn blockOf(v: usize, p: usize, q: usize) block {
    return (v * size + p) * size + q;
}

fn coordOf(i: usize, j: usize, k: usize) coord {
    return (i * size + j) * size + k;
}

const blocks_of: [n_coords][4]block = blk: {
    @setEvalBranchQuota(10000);
    var tmp: [n_coords][4]block = undefined;
    for (0..size) |i| {
        for (0..size) |j| {
            const p = i / unit * unit + j / unit;
            for (0..size) |k| {
                tmp[coordOf(i, j, k)] = .{
                    blockOf(0, i, j),
                    blockOf(1, i, k),
                    blockOf(2, j, k),
                    blockOf(3, p, k),
                };
            }
        }
    }
    break :blk tmp;
};

const coords_of: [n_blocks][size]coord = blk: {
    @setEvalBranchQuota(10000);
    var tmp: [n_blocks][size]coord = undefined;
    var cnt: [n_blocks]usize = .{0} ** n_blocks;
    for (0..n_coords) |c| {
        for (blocks_of[c]) |b| {
            tmp[b][cnt[b]] = c;
            cnt[b] += 1;
        }
    }
    break :blk tmp;
};

const State = enum { undefined, true, false };

const Counter = BinaryHeapCounter(u8, n_blocks);

fn BinaryHeapCounter(comptime T: type, comptime len: usize) type {
    return struct {
        value: [len]T,
        order: [len]usize,
        index: [len]usize,
        const Self = @This();
        const infinity = std.math.maxInt(T);
        fn init(init_value: T) Self {
            var self: Self = undefined;
            for (0..len) |i| {
                self.value[i] = init_value;
                self.order[i] = i;
                self.index[i] = i;
            }
            return self;
        }
        fn empty(self: *Self) bool {
            return self.get(self.argmin()) == infinity;
        }
        fn get(self: *Self, i: usize) T {
            return self.value[i];
        }
        fn argmin(self: *Self) usize {
            return self.index[0];
        }
        fn decrement(self: *Self, i: usize) void {
            self.value[i] -= 1;
            self.upsort(self.order[i]);
        }
        fn remove(self: *Self, i: usize) void {
            self.value[i] = infinity;
            self.downsort(self.order[i]);
        }
        fn upsort(self: *Self, o: usize) void {
            if (o == 0) return;
            const p = (o - 1) / 2;
            if (self.value[self.index[p]] <= self.value[self.index[o]]) return;
            self.swap(p, o);
            self.upsort(p);
        }
        fn downsort(self: *Self, o: usize) void {
            const o1 = o * 2 + 1;
            const o2 = o * 2 + 2;
            const v = self.value[self.index[o]];
            const v1 = if (o1 < len) self.value[self.index[o1]] else infinity;
            const v2 = if (o2 < len) self.value[self.index[o2]] else infinity;
            if (v <= v1 and v <= v2) return;
            if (v1 < v and v1 <= v2) {
                self.swap(o, o1);
                self.downsort(o1);
                return;
            }
            if (v2 < v and v2 < v1) {
                self.swap(o, o2);
                self.downsort(o2);
                return;
            }
            unreachable;
        }
        fn swap(self: *Self, o1: usize, o2: usize) void {
            const j1 = self.index[o1];
            const j2 = self.index[o2];
            self.index[o1] = j2;
            self.index[o2] = j1;
            self.order[j1] = o2;
            self.order[j2] = o1;
        }
    };
}
