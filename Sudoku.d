import std.conv: to;
import std.range.primitives: front, popFront;
import std.stdio: readln, writeln;
import std.string: chomp, indexOf;

void main() {
    Sudoku.init();
    string line;
    while ((line = readln()) !is null)
        writeln(Sudoku.solve(chomp(line)));
}

class Sudoku {

    static string solve(string input) {
        try {
            return new Sudoku().fix(input).search().show();
        }
        catch (NoSolutionException) {
            return "NO SOLUTION";
        }
    }

    enum View { GRD, ROW, COL, BOX }
    enum State { OPEN, FIXED, BANNED }
    alias Coord = int;
    alias Block = int;
    alias Count = int;

    static immutable int UNIT = 3;
    static immutable int SIZE = UNIT * UNIT;
    static immutable int COORD = SIZE * SIZE * SIZE;
    static immutable int BLOCK = 4 * SIZE * SIZE;

    static immutable Count DONE = SIZE + 1;

    static immutable string DIGITS = "123456789";

    static Coord coord(int i, int j, int k) {
        return (i * SIZE + j) * SIZE + k;
    }

    static Block block(View v, int i, int j) {
        return ((cast(int) v) * SIZE + i) * SIZE + j;
    }

    static Block[][COORD] parents;
    static Coord[][BLOCK] children;

    static init() {
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                int p = i / UNIT * UNIT + j / UNIT;
                for (int k = 0; k < SIZE; k++)
                    parents[coord(i, j, k)] = [
                        block(View.GRD, i, j),
                        block(View.ROW, i, k),
                        block(View.COL, j, k),
                        block(View.BOX, p, k),
                    ];
            }
        }
        for (Coord c = 0; c < COORD; c++)
            foreach (Block b; parents[c])
                children[b] ~= c;
    }

    class NoSolutionException : Exception {
        this() {
            super("NO SOLUTION");
        }
    }

    State[COORD] state;
    Count[BLOCK] count;
    Coord[] queue;

    this() {
        state[] = State.OPEN;
        count[] = SIZE;
    }

    this(const Sudoku that) {
        state[] = that.state[];
        count[] = that.count[];
    }

    Sudoku fix(Coord c) {
        queue ~= c;
        return this;
    }

    Sudoku fix(string input) {
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                if (i * SIZE + j < input.length) {
                    int k = cast(int) indexOf(DIGITS, input[i * SIZE + j]);
                    if (k != -1)
                        fix(coord(i, j, k));
                }
            }
        }
        return this;
    }

    string show() {
        char[SIZE * SIZE] result;
        result[] = '.';
        for (int i = 0; i < SIZE; i++)
            for (int j = 0; j < SIZE; j++)
                for (int k = 0; k < SIZE; k++)
                    if (state[coord(i, j, k)] == State.FIXED)
                        result[i * SIZE + j] = DIGITS[k];
        return to!string(result);
    }

    Sudoku search() {

        while (queue.length > 0) {
            Coord c0 = queue.front();
            queue.popFront();
            if (state[c0] == State.BANNED)
                throw new NoSolutionException;
            if (state[c0] == State.FIXED)
                continue;
            state[c0] = State.FIXED;
            foreach (Block b1; parents[c0]) {
                count[b1] = DONE;
                foreach (Coord c2; children[b1]) {
                    if (c2 != c0 && state[c2] == State.OPEN) {
                        state[c2] = State.BANNED;
                        foreach (Block b3; parents[c2]) {
                            if (b3 != b1) {
                                count[b3]--;
                                if (count[b3] == 0)
                                    throw new NoSolutionException;
                                if (count[b3] == 1)
                                    foreach (Coord c4; children[b3])
                                        if (state[c4] == State.OPEN)
                                            fix(c4);
                            }
                        }
                    }
                }
            }
        }

        Count min = DONE;
        Block bmin;
        for (Block b = 0; b < BLOCK; b++) {
            if (count[b] < min) {
                min = count[b];
                bmin = b;
            }
        }

        if (min == DONE)
            return this;

        foreach (Coord c; children[bmin]) {
            if (state[c] == State.OPEN) {
                try {
                    Sudoku s = new Sudoku(this);
                    return s.fix(c).search();
                }
                catch (NoSolutionException) {
                }
            }
        }

        throw new NoSolutionException;
    }
}
