#include <iostream>
#include <array>
#include <vector>
#include <queue>
using namespace std;

typedef int Coord;
typedef int Block;
typedef int Count;

enum View { GRD, ROW, COL, BOX };
enum State { OPEN, FIXED, BANNED };

const int VIEW  = 4;
const int UNIT  = 3;
const int SIZE  = UNIT * UNIT;
const int COORD = SIZE * SIZE * SIZE;
const int BLOCK = VIEW * SIZE * SIZE;

const Count DONE = SIZE + 1;

const string DIGITS = "123456789";

array<vector<Block>, COORD> parents;
array<vector<Coord>, BLOCK> children;

inline Coord coord(int i, int j, int k) {
    return (i * SIZE + j) * SIZE + k;
}

inline Block block(View v, int p, int q) {
    return (v * SIZE + p) * SIZE + q;
}

void init() {
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            int p = i / UNIT * UNIT + j / UNIT;
            for (int k = 0; k < SIZE; k++) {
                vector<Block>& vec = parents[coord(i, j, k)];
                vec.push_back(block(GRD, i, j));
                vec.push_back(block(ROW, i, k));
                vec.push_back(block(COL, j, k));
                vec.push_back(block(BOX, p, k));
            }
        }
    }
    for (Coord c = 0; c < COORD; c++)
        for (Block b : parents[c])
            children[b].push_back(c);
}

class Sudoku {

    public:

        static string solve(string input) {
            Sudoku s;
            try {
                return s.read(input).search().show();
            }
            catch (NoSolutionException) {
                return "NO SOLUTION";
            }
        }

    private:

        static const class NoSolutionException {} NO_SOLUTION_EXCEPTION;

        array<State, COORD> state;
        array<Count, BLOCK> count;
        queue<Coord>        to_fix;

        Sudoku() {
            state.fill(OPEN);
            count.fill(SIZE);
        }

        Sudoku(const Sudoku& that) {
            state = that.state;
            count = that.count;
        }

        Sudoku& fix(Coord c) {
            to_fix.push(c);
            return *this;
        }

        Sudoku& read(string input) {
            int len = input.length();
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    if (i * SIZE + j < len) {
                        int k = DIGITS.find(input[i * SIZE + j]);
                        if (k != -1)
                            fix(coord(i, j, k));
                    }
                }
            }
            return *this;
        }

        Sudoku search() {

            while (not to_fix.empty()) {
                Coord c0 = to_fix.front();
                to_fix.pop();
                if (state[c0] == FIXED)
                    continue;
                if (state[c0] == BANNED)
                    throw NO_SOLUTION_EXCEPTION;
                state[c0] = FIXED;
                for (Block b1 : parents[c0]) {
                    count[b1] = DONE;
                    for (Coord c2 : children[b1]) {
                        if (c2 != c0 && state[c2] == OPEN) {
                            state[c2] = BANNED;
                            for (Block b3 : parents[c2]) {
                                if (b3 != b1) {
                                    count[b3]--;
                                    if (count[b3] == 0)
                                        throw NO_SOLUTION_EXCEPTION;
                                    if (count[b3] == 1)
                                        for (Coord c4 : children[b3])
                                            if (state[c4] == OPEN)
                                                fix(c4);
                                }
                            }
                        }
                    }
                }
            }

            Count min = DONE;
            Block bmin = -1;
            for (Block b = 0; b < BLOCK; b++) {
                if (count[b] < min) {
                    min = count[b];
                    bmin = b;
                }
            }

            if (min == DONE)
                return *this;

            for (Coord c : children[bmin]) {
                if (state[c] == OPEN) {
                    Sudoku s(*this);
                    try {
                        return s.fix(c).search();
                    }
                    catch (NoSolutionException) {
                    }
                }
            }

            throw NO_SOLUTION_EXCEPTION;
        }

        string show() {
            string output(SIZE * SIZE, '.');
            for (int i = 0; i < SIZE; i++)
                for (int j = 0; j < SIZE; j++)
                    for (int k = 0; k < SIZE; k++)
                        if (state[coord(i, j, k)] == FIXED)
                            output[i * SIZE + j] = DIGITS[k];
            return output;
        }
};

int main() {
    init();
    string input;
    while (cin >> input)
        cout << Sudoku::solve(input) << endl;
    return 0;
}
