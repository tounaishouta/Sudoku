#include <iostream>
#include <array>
#include <vector>
#include <queue>
using namespace std;

enum View { GRD, ROW, COL, BOX };
enum CState { TBD, FIX, BAN };

typedef int Coord;
typedef int Block;
typedef int BState;

const int VIEW  = 4;
const int UNIT  = 3;
const int SIZE  = UNIT * UNIT;
const int COORD = SIZE * SIZE * SIZE;
const int BLOCK = VIEW * SIZE * SIZE;

const BState DONE = SIZE + 1;

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
                Coord c = coord(i, j, k);
                parents[c].push_back(block(GRD, i, j));
                parents[c].push_back(block(ROW, i, k));
                parents[c].push_back(block(COL, j, k));
                parents[c].push_back(block(BOX, p, k));
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

        array<CState, COORD> cstate;
        array<BState, BLOCK> bstate;
        queue<Coord>         to_fix;

        Sudoku() {
            cstate.fill(TBD);
            bstate.fill(SIZE);
        }

        Sudoku(const Sudoku& that) {
            cstate = that.cstate;
            bstate = that.bstate;
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

        Sudoku& search() {

            while (not to_fix.empty()) {
                Coord c0 = to_fix.front();
                to_fix.pop();
                if (cstate[c0] == FIX)
                    continue;
                if (cstate[c0] == BAN)
                    throw NO_SOLUTION_EXCEPTION;
                cstate[c0] = FIX;
                for (Block b1 : parents[c0]) {
                    bstate[b1] = DONE;
                    for (Coord c2 : children[b1]) {
                        if (c2 != c0 && cstate[c2] != BAN) {
                            cstate[c2] = BAN;
                            for (Block b3 : parents[c2]) {
                                if (b3 != b1) {
                                    bstate[b3]--;
                                    if (bstate[b3] == 0)
                                        throw NO_SOLUTION_EXCEPTION;
                                    if (bstate[b3] == 1)
                                        for (Coord c4 : children[b3])
                                            if (cstate[c4] != BAN)
                                                fix(c4);
                                }
                            }
                        }
                    }
                }
            }

            BState min = DONE;
            Block bmin = -1;
            for (Block b = 0; b < BLOCK; b++) {
                if (bstate[b] < min) {
                    min = bstate[b];
                    bmin = b;
                }
            }

            // summary();

            if (min == DONE)
                return *this;

            for (Coord c : children[bmin]) {
                if (cstate[c] != BAN) {
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

            // summary();

            string output(SIZE * SIZE, '.');
            for (int i = 0; i < SIZE; i++)
                for (int j = 0; j < SIZE; j++)
                    for (int k = 0; k < SIZE; k++)
                        if (cstate[coord(i, j, k)] == FIX)
                            output[i * SIZE + j] = DIGITS[k];
            return output;
        }

        void summary() {
            cout << endl;
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    for (int k = 0; k < SIZE; k++) {
                        switch (cstate[coord(i, j, k)]) {
                            case TBD: cout << '.'; break;
                            case FIX: cout << 'o'; break;
                            case BAN: cout << 'x'; break;
                            default:  cout << '?'; break;
                        }
                    }
                    cout << ' ';
                }
                cout << endl;
            }
            cout << endl;
            for (int v = 0; v < VIEW; v++) {
                for (int p = 0; p < SIZE; p++) {
                    for (int q = 0; q < SIZE; q++) {
                        BState val = bstate[block(View(v), p, q)];
                        if (0 <= val && val <= SIZE)
                            cout << val;
                        else if (val == DONE)
                            cout << 'o';
                        else
                            cout << '?';
                    }
                    cout << ' ';
                }
                cout << endl;
            }
            cout << endl;
        }
};

int main() {
    init();
    string input;
    while (cin >> input)
        cout << Sudoku::solve(input) << endl;
    return 0;
}
