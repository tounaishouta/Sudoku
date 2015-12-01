#include <array>
#include <bitset>
#include <list>
#include <iostream>
#include <set>
using namespace std;

class sudoku {
    public:
        static class no_solution {} NO_SOLUTION;
        sudoku() {
            if (not initialized)
                initialize();
            clear();
        }
        sudoku(const sudoku & s) {
            clone(s);
        }
        sudoku & clear() {
            for (int b = 0; b < N_BLOCK; b++)
                n_option[b] = N_DIGIT;
            for (int c = 0; c < N_COORD; c++)
                admissible[c] = true;
            return *this;
        }
        sudoku & read(string str) {
            string::iterator its = str.begin();
            for (int i = 0; i < N_DIGIT; i++) {
                for (int j = 0; j < N_DIGIT; j++) {
                    if (its == str.end())
                        return *this;
                    if (is_digit(*its))
                        assign(to_coord(i, j, to_digit(*its)));
                    its++;
                }
            }
            return *this;
        }
        sudoku & solve() {
            int b_min = -1;
            int n_min = DEFINED;
            for (int b = 0; b < N_BLOCK; b++) {
                if (n_option[b] < n_min) {
                    b_min = b;
                    n_min = n_option[b_min];
                }
            }
            if (n_min == DEFINED)
                return *this;
            sudoku now = *this;
            for (int c : children[b_min]) {
                if (now.admissible[c]) {
                    try {
                        return clone(now).assign(c).solve();
                    }
                    catch (no_solution) {
                        continue;
                    }
                }
            }
            throw NO_SOLUTION;
        }
        string show(string delim = "") {
            string str;
            for (int i = 0; i < N_DIGIT; i++) {
                for (int j = 0; j < N_DIGIT; j++) {
                    list<int> ks;
                    for (int k = 0; k < N_DIGIT; k++)
                        if (admissible[to_coord(i, j, k)])
                            ks.push_back(k);
                    if (ks.size() == 1)
                        str += from_digit(ks.front());
                    else
                        str += '.';
                }
                str += delim;
            }
            return str;
        }
    private:
        enum view { GRD = 0, ROW = 1, COL = 2, BOX = 3, N_VIEW = 4 };
        static const int SIZE = 3;
        static const int N_DIGIT = SIZE * SIZE;
        static const int N_BLOCK = N_VIEW * N_DIGIT * N_DIGIT;
        static const int N_COORD = N_DIGIT * N_DIGIT * N_DIGIT;
        static const int DEFINED = 0xDEF;
        static inline bool is_digit(char c) {
            return '1' <= c and c <= '9';
        }
        static inline char from_digit(int k) {
            return '1' + k;
        }
        static inline int to_digit(char c) {
            return c - '1';
        }
        static inline int to_block(view v, int p, int q) {
            return (v * N_DIGIT + p) * N_DIGIT + q;
        }
        static inline int to_coord(int i, int j, int k) {
            return (i * N_DIGIT + j) * N_DIGIT + k;
        }
        static array<list<int>, N_COORD> parents;
        static array<list<int>, N_BLOCK> children;
        static array<list<int>, N_COORD> siblings;
        static bool initialized;
        static void initialize() {
            for (int i = 0; i < N_DIGIT; i++) {
                for (int j = 0; j < N_DIGIT; j++) {
                    for (int k = 0; k < N_DIGIT; k++) {
                        int c = to_coord(i, j, k);
                        int p = i / SIZE * SIZE + j / SIZE;
                        parents[c].push_back(to_block(GRD, i, j));
                        parents[c].push_back(to_block(ROW, i, k));
                        parents[c].push_back(to_block(COL, j, k));
                        parents[c].push_back(to_block(BOX, p, k));
                    }
                }
            }
            for (int c = 0; c < N_COORD; c++)
                for (int b : parents[c])
                    children[b].push_back(c);
            for (int c = 0; c < N_COORD; c++) {
                set<int> cs;
                for (int b : parents[c])
                    for (int cc : children[b])
                        cs.insert(cc);
                cs.erase(c);
                for (int cc : cs)
                    siblings[c].push_back(cc);
            }
        }
        array<int, N_BLOCK> n_option;
        bitset<N_COORD> admissible;
        sudoku & clone(const sudoku & s) {
            for (int b = 0; b < N_BLOCK; b++)
                n_option[b] = s.n_option[b];
            for (int c = 0; c < N_COORD; c++)
                admissible[c] = s.admissible[c];
            return *this;
        }
        sudoku & assign(int c) {
            set<int> bs;
            for (int cc : siblings[c]) {
                if (admissible[cc]) {
                    admissible[cc] = false;
                    for (int b : parents[cc]) {
                        n_option[b]--;
                        bs.insert(b);
                    }
                }
            }
            for (int b : parents[c]) {
                n_option[b] = DEFINED;
                bs.erase(b);
            }
            for (int b : bs)
                check(b);
            return *this;
        }
        sudoku & check(int b) {
            if (n_option[b] == 0) {
                throw NO_SOLUTION;
            }
            else if (n_option[b] == 1) {
                int c_unique = -1;
                for (int c : children[b])
                    if (admissible[c])
                        c_unique = c;
                return assign(c_unique);
            }
            else {
                return *this;
            }
        }
};

array<list<int>, sudoku::N_COORD> sudoku::parents;
array<list<int>, sudoku::N_BLOCK> sudoku::children;
array<list<int>, sudoku::N_COORD> sudoku::siblings;
bool sudoku::initialized = false;

int main() {
    sudoku s;
    string str;
    while (true) {
        cin >> str;
        if (cin.eof())
            break;
        try {
            cout << s.clear().read(str).solve().show() << endl;
        }
        catch (sudoku::no_solution) {
            cout << "no solution" << endl;
        }
    }
    return 0;
}
