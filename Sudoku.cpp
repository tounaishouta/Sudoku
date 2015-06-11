#include <iostream>
#include <bitset>
#include <array>
#include <list>
#include <set>

using namespace std;

const int SIZE    = 3;
const int N_DIGIT = SIZE * SIZE;
const int N_COORD = N_DIGIT * N_DIGIT * N_DIGIT;
const int N_BLOCK = 4 * N_DIGIT * N_DIGIT;
const int DEFINED = 0xDEF;

inline int to_coord(int i, int j, int d) {
    return (i * N_DIGIT + j) * N_DIGIT + d;
}
inline int to_block(int v, int p, int q) {
    return (v * N_DIGIT + p) * N_DIGIT + q;
}
inline bool is_digit(char c) {
    return '1' <= c and c <= '9';
}
inline int to_digit(char c) {
    return c - '1';
}
inline char to_char(int d) {
    return '1' + d;
}

array<list<int>, N_BLOCK> members;
array<list<int>, N_COORD> owners;
array<list<int>, N_COORD> antis;

inline void initialize() {
    for (int i = 0; i < N_DIGIT; i++) {
        for (int j = 0; j < N_DIGIT; j++) {
            for (int d = 0; d < N_DIGIT; d++) {
                int c = to_coord(i, j, d);
                int p = i / SIZE * SIZE + j / SIZE;
                members[to_block(0, i, j)].push_back(c);
                members[to_block(1, i, d)].push_back(c);
                members[to_block(2, j, d)].push_back(c);
                members[to_block(3, p, d)].push_back(c);
            }
        }
    }
    for (int b = 0; b < N_BLOCK; b++)
        for (int c : members[b])
            owners[c].push_back(b);
    for (int c = 0; c < N_COORD; c++) {
        set<int> cs;
        for (int b : owners[c])
            for (int cc : members[b])
                cs.insert(cc);
        cs.erase(c);
        for (int cc : cs)
            antis[c].push_back(cc);
    }
}

class no_solution {} NO_SOLUTION;

class grid {
    private:
        bitset<N_COORD>     possible;
        array<int, N_BLOCK> n_option;
    public:
        grid() {
            clear();
        }
        grid(const grid & old) {
            copy(old);
        }
        grid & clear() {
            for (int c = 0; c < N_COORD; c++)
                possible[c] = true;
            for (int b = 0; b < N_BLOCK; b++)
                n_option[b] = N_DIGIT;
            return *this;
        }
        grid & copy(const grid & old) {
            for (int c = 0; c < N_COORD; c++)
                possible[c] = old.possible[c];
            for (int b = 0; b < N_BLOCK; b++)
                n_option[b] = old.n_option[b];
            return *this;
        }
        grid & solve() {
            int b_min = -1;
            int min = DEFINED;
            for (int b = 0; b < N_BLOCK; b++) {
                if (n_option[b] < min) {
                    min = n_option[b];
                    b_min = b;
                }
            }
            if (min == DEFINED)
                return *this;
            grid saved = *this;
            for (int c : members[b_min]) {
                if (saved.possible[c]) {
                    try {
                        return copy(saved).assign(c).solve();
                    }
                    catch (no_solution) {
                        continue;
                    }
                }
            }
            throw NO_SOLUTION;
        }
        grid & read(istream & in) {
            char c;
            for (int i = 0; i < N_DIGIT; i++) {
                for (int j = 0; j < N_DIGIT; j++) {
                    in >> c;
                    if (is_digit(c))
                        assign(to_coord(i, j, to_digit(c)));
                }
            }
            return *this;
        }
        string show() const {
            string s;
            for (int i = 0; i < N_DIGIT; i++) {
                for (int j = 0; j < N_DIGIT; j++) {
                    int count = 0;
                    int d_possible = -1;
                    for (int d = 0; d < N_DIGIT; d++) {
                        if (possible[to_coord(i, j, d)]) {
                            count++;
                            d_possible = d;
                        }
                    }
                    if (count == 1)
                        s += to_char(d_possible);
                    else
                        s+= '.';
                }
                s += '\n';
            }
            return s;
        }
    private:
        grid & assign(int c) {
            if (!possible[c])
                throw NO_SOLUTION;
            list<int> bs;
            for (int cc : antis[c]) {
                if (possible[cc]) {
                    possible[cc] = false;
                    for (int b : owners[cc]) {
                        bs.push_back(b);
                        n_option[b]--;
                    }
                }
            }
            for (int b : owners[c])
                n_option[b] = DEFINED;
            for (int b : bs)
                check(b);
            return *this;
        }
        grid & check(int b) {
            if (n_option[b] == 0)
                throw NO_SOLUTION;
            if (n_option[b] > 1)
                return *this;
            int c_possible = -1;
            for (int c : members[b])
                if (possible[c])
                    c_possible = c;
            return assign(c_possible);
        }
} g;

int main() {
    initialize();
    cout << g.read(cin).solve().show() << endl;
    return 0;
}
