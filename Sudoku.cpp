#include <iostream>
#include <set>
#include <vector>
using namespace std;

typedef int coord;
typedef int block;

enum view { GRD = 0, ROW = 1, COL = 2, BOX = 3 };
const int SIZE    = 3;
const int N_DIGIT = SIZE * SIZE;
const int N_COORD = N_DIGIT * N_DIGIT * N_DIGIT;
const int N_VIEW  = 4;
const int N_BLOCK = N_VIEW * N_DIGIT * N_DIGIT;
const int DEFINED = 0xDEF;

inline bool is_digit(char ch) {
    return '1' <= ch && ch <= '9';
}
inline int digit_of(char ch) {
    return ch - '1';
}
inline char char_of(int i) {
    return '1' + i;
}
inline coord coord_of(int i, int j, int k) {
    return (i * N_DIGIT + j) * N_DIGIT + k;
}
inline block block_of(view v, int i, int j) {
    return (v * N_DIGIT + i) * N_DIGIT + j;
}

vector<block> parents[N_COORD];
vector<coord> children[N_BLOCK];
vector<coord> siblings[N_COORD];

void initialize() {
    for (int i = 0; i < N_DIGIT; i++) {
        for (int j = 0; j < N_DIGIT; j++) {
            int p = i / SIZE * SIZE + j / SIZE;
            for (int k = 0; k < N_DIGIT; k++) {
                coord c = coord_of(i, j, k);
                parents[c].push_back(block_of(GRD, i, j));
                parents[c].push_back(block_of(ROW, i, k));
                parents[c].push_back(block_of(COL, j, k));
                parents[c].push_back(block_of(BOX, p, k));
            }
        }
    }
    for (coord c = 0; c < N_COORD; c++) {
        for (block b : parents[c]) {
            children[b].push_back(c);
        }
    }
    set<coord> cset;
    for (coord c = 0; c < N_COORD; c++) {
        cset.clear();
        for (block b : parents[c]) {
            for (coord cc : children[b]) {
                cset.insert(cc);
            }
        }
        cset.erase(c);
        for (coord cc : cset) {
            siblings[c].push_back(cc);
        }
    }
}

class no_solution {} NO_SOLUTION;

class sudoku {
    private:
        bool admits[N_COORD];
        int  option[N_BLOCK];
        sudoku& assign(coord c) {
            if (not admits[c]) {
                throw NO_SOLUTION;
            }
            set<block> bset;
            for (coord cc : siblings[c]) {
                if (admits[cc]) {
                    admits[cc] = false;
                    for (block b : parents[cc]) {
                        bset.insert(b);
                        option[b]--;
                    }
                }
            }
            for (block b : parents[c]) {
                option[b] = DEFINED;
            }
            for (block b : bset) {
                check(b);
            }
            return *this;
        }
        sudoku& check(block b) {
            switch (option[b]) {
                case 0:
                    throw NO_SOLUTION;
                case 1:
                    for (coord c : children[b]) {
                        if (admits[c]) {
                            return assign(c);
                        }
                    }
                    throw "something wrong";
                default:
                    return *this;
            }
        }
    public:
        sudoku() {
            clear();
        }
        sudoku(const sudoku& that) {
            copy(that);
        }
        sudoku& clear() {
            for (coord c = 0; c < N_COORD; c++) {
                admits[c] = true;
            }
            for (block b = 0; b < N_BLOCK; b++) {
                option[b] = N_DIGIT;
            }
            return *this;
        }
        sudoku& copy(const sudoku& that) {
            for (coord c = 0; c < N_COORD; c++) {
                admits[c] = that.admits[c];
            }
            for (block b = 0; b < N_BLOCK; b++) {
                option[b] = that.option[b];
            }
            return *this;
        }
        sudoku& read(string input) {
            for (int p = 0, l = input.length(); p < l && p < N_DIGIT * N_DIGIT; p++) {
                char ch = input[p];
                if (not is_digit(ch)) {
                    continue;
                }
                int i = p / N_DIGIT;
                int j = p % N_DIGIT;
                int k = digit_of(ch);
                assign(coord_of(i, j, k));
            }
            return *this;
        }
        sudoku& solve() {
            int minimum_option = DEFINED;
            for (int o : option) {
                if (o < minimum_option) {
                    minimum_option = o;
                }
            }
            if (minimum_option == DEFINED) {
                return *this;
            }
            block b = 0;
            while (option[b] != minimum_option) {
                b++;
            }
            sudoku temp(*this);
            for (coord c : children[b]) {
                try {
                    return copy(temp).assign(c).solve();
                }
                catch (no_solution e){
                    continue;
                }
            }
            throw NO_SOLUTION;
        }
        string show() {
            string output = "";
            for (int i = 0; i < N_DIGIT; i++) {
                for (int j = 0; j < N_DIGIT; j++) {
                    if (option[block_of(GRD, i, j)] == DEFINED) {
                        int k = 0;
                        while (not admits[coord_of(i, j, k)]) {
                            k++;
                        }
                        output += char_of(k);
                    }
                    else {
                        output += '.';
                    }
                }
            }
            return output;
        }
};

int main() {
    initialize();
    string input;
    sudoku s;
    while (true) {
        cin >> input;
        if (cin.eof()) {
            return 0;
        }
        try {
            cout << s.clear().read(input).solve().show() << endl;
        }
        catch (no_solution e) {
            cout << "no solution" << endl;
        }
    }
}
