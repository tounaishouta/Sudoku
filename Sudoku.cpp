#include <iostream>
#include <queue>
#include <string>
#include <vector>
using namespace std;

typedef int coord;
typedef int block;

enum view { GRD, ROW, COL, BOX };

const int VIEW    = 4;
const int UNIT    = 3;
const int SIZE    = UNIT * UNIT;
const int COORD   = SIZE * SIZE * SIZE;
const int BLOCK   = VIEW * SIZE * SIZE;
const int DEFINED = 0xDEF;

const string DIGITS = "123456789";

inline coord coord_of(int i, int j, int k) {
    return (i * SIZE + j) * SIZE + k;
}

inline block block_of(view v, int p, int q) {
    return (v * SIZE + p) * SIZE + q;
}

vector<block> parents[COORD];
vector<coord> children[BLOCK];

void init() {
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            int p = i / UNIT * UNIT + j / UNIT;
            for (int k = 0; k < SIZE; k++) {
                coord c = coord_of(i, j, k);
                parents[c].push_back(block_of(GRD, i, j));
                parents[c].push_back(block_of(ROW, i, k));
                parents[c].push_back(block_of(COL, j, k));
                parents[c].push_back(block_of(BOX, p, k));
            }
        }
    }
    for (coord c = 0; c < COORD; c++)
        for (block b : parents[c])
            children[b].push_back(c);
}

class no_solution_exception {} NO_SOLUTION_EXCEPTION;

class sudoku {

    public:

        static string solve(string input) {
            try {
                sudoku s;
                return s.read(input).search().to_string();
            }
            catch (no_solution_exception) {
                return "NO SOLUTION";
            }
        }

    private:

        bool admit[COORD];

        int count[BLOCK];

        sudoku() {
            for (coord c = 0; c < COORD; c++)
                admit[c] = true;
            for (block b = 0; b < BLOCK; b++)
                count[b] = SIZE;
        }

        sudoku(const sudoku& s) {
            for (coord c = 0; c < COORD; c++)
                admit[c] = s.admit[c];
            for (block b = 0; b < BLOCK; b++)
                count[b] = s.count[b];
        }

        sudoku& search() {

            int min = DEFINED;
            int bmin = -1;
            for (block b = 0; b < BLOCK; b++) {
                if (count[b] < min) {
                    min = count[b];
                    bmin = b;
                }
            }

            if (min == DEFINED)
                return *this;

            for (coord c : children[bmin]) {
                if (admit[c]) {
                    try {
                        sudoku s(*this);
                        return s.assign(c).search();
                    }
                    catch (no_solution_exception) {
                    }
                }
            }

            throw NO_SOLUTION_EXCEPTION;
        }

        sudoku& read(string input) {
            int l = min(int(input.length()), SIZE * SIZE);
            for (int ij = 0; ij < l; ij++) {
                int k = DIGITS.find(input[ij]);
                if (k != -1)
                    assign(coord_of(ij / SIZE, ij % SIZE, k));
            }
            return *this;
        }


        sudoku& assign(coord c) {
            queue<coord> queue;
            queue.push(c);
            while (not queue.empty()) {
                int c0 = queue.front();
                queue.pop();
                if (not admit[c0])
                    throw NO_SOLUTION_EXCEPTION;
                for (block b1 : parents[c0]) {
                    count[b1] = DEFINED;
                    for (coord c2 : children[b1]) if (c2 != c0 and admit[c2]) {
                        admit[c2] = false;
                        for (block b3 : parents[c2]) if (b3 != b1) {
                            count[b3]--;
                            if (count[b3] == 0)
                                throw NO_SOLUTION_EXCEPTION;
                            if (count[b3] == 1)
                                for (coord c4 : children[b3]) if (admit[c4])
                                    queue.push(c4);
                        }
                    }
                }
            }
            return *this;
        }

        string to_string() {
            string output;
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    vector<int> ks;
                    for (int k = 0; k < SIZE; k++)
                        if (admit[coord_of(i, j, k)])
                            ks.push_back(k);
                    if (ks.size() == 1)
                        output += DIGITS[ks[0]];
                    else
                        output += '.';
                }
            }
            return output;
        }
};

int main() {
    init();
    string input;
    while (cin >> input)
        cout << sudoku::solve(input) << endl;
    return 0;
}
