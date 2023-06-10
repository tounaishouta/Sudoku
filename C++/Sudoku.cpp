#include <iostream>
#include <array>
#include <deque>
#include <vector>
using namespace std;

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

        static void init() {
            parents.resize(COORD);
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
            children.resize(BLOCK);
            for (Coord c = 0; c < COORD; c++)
                for (Block b : parents[c])
                    children[b].push_back(c);
        }

    private:

        enum View { GRD, ROW, COL, BOX };
        enum State { OPEN, FIXED, BANNED };

        typedef int Coord;
        typedef int Block;
        typedef int Count;

        static const int UNIT = 3;
        static const int SIZE = UNIT * UNIT;
        static const int COORD = SIZE * SIZE * SIZE;
        static const int BLOCK = 4 * SIZE * SIZE;
        static const Count DONE = SIZE + 1;
        static const string DIGITS;

        static vector<vector<Block>> parents;
        static vector<vector<Coord>> children;

        static class NoSolutionException {} NO_SOLUTION_EXCEPTION;

        static Coord coord(int i, int j, int k) {
            return (i * SIZE + j) * SIZE + k;
        }

        static Block block(View v, int p, int q) {
            return (v * SIZE + p) * SIZE + q;
        }

        array<State, COORD> state;
        array<Count, BLOCK> count;
        deque<Coord> queue;

        Sudoku() {
            state.fill(OPEN);
            count.fill(SIZE);
        }

        Sudoku(const Sudoku& that) {
            state = that.state;
            count = that.count;
        }

        Sudoku& enqueue(Coord c) {
            queue.push_back(c);
            return *this;
        }

        Sudoku& read(string input) {
            int len = input.length();
            for (int i = 0; i < SIZE; i++) {
                for (int j = 0; j < SIZE; j++) {
                    if (i * SIZE + j < len) {
                        int k = DIGITS.find(input[i * SIZE + j]);
                        if (k != -1)
                            enqueue(coord(i, j, k));
                    }
                }
            }
            return *this;
        }

        string show() {
            string result(SIZE * SIZE, '.');
            for (int i = 0; i < SIZE; i++)
                for (int j = 0; j < SIZE; j++)
                    for (int k = 0; k < SIZE; k++)
                        if (state[coord(i, j, k)] == FIXED)
                            result[i * SIZE + j] = DIGITS[k];
            return result;
        }

        Sudoku search() {

            while (not queue.empty()) {
                Coord c = queue.front();
                queue.pop_front();
                if (state[c] == FIXED)
                    continue;
                if (state[c] == BANNED)
                    throw NO_SOLUTION_EXCEPTION;
                fix(c);
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
                return *this;

            for (Coord c : children[bmin]) {
                if (state[c] == BANNED)
                    continue;
                Sudoku s(*this);
                try {
                    return s.enqueue(c).search();
                }
                catch (NoSolutionException) {
                }
            }

            throw NO_SOLUTION_EXCEPTION;
        }

        void fix(Coord c) {
            state[c] = FIXED;
            for (Block b : parents[c])
                mark_done(b);
        }

        void mark_done(Block b) {
            count[b] = DONE;
            for (Coord c : children[b])
                if (state[c] == OPEN)
                    ban(c);
        }

        void ban(Coord c) {
            state[c] = BANNED;
            for (Block b : parents[c])
                if (count[b] != DONE)
                    countdown(b);
        }

        void countdown(Block b) {
            count[b]--;
            if (count[b] == 0)
                throw NO_SOLUTION_EXCEPTION;
            if (count[b] == 1)
                for (Coord c : children[b])
                    if (state[c] == OPEN)
                        enqueue(c);
        }
};

const string Sudoku::DIGITS = "123456789";

vector<vector<Sudoku::Block>> Sudoku::parents;
vector<vector<Sudoku::Coord>> Sudoku::children;

int main() {
    Sudoku::init();
    string input;
    while (cin >> input)
        cout << Sudoku::solve(input) << endl;
}
