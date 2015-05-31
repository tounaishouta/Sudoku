using namespace std;

#include <iostream>
#include <bitset>
#include <array>
#include <list>
#include <set>

#define digit int
#define coord int
#define block int

#define _box_   3
#define _digit_ 9   // = _box_ * _box_
#define _coord_ 729 // = _digit_ * _digit_ * _digit_
#define _block_ 324 // = 4 * _digit_ * _digit_

#define GRD 0
#define ROW 1
#define COL 2
#define BOX 3

#define DEFINED 999
#define INFINITY 99

#define to_coord(i, j, d) (i * _digit_ + j) * _digit_ + d
#define to_block(v, p, q) (v * _digit_ + p) * _digit_ + q

#define for_digit(d)      for (digit d = 0; d < _digit_; d++)
#define for_coord(c)      for (coord c = 0; c < _coord_; c++)
#define for_block(b)      for (block b = 0; b < _block_; b++)
#define for_each(xs, itx) for (auto itx = xs.begin(); itx != xs.end(); itx++)

class Grid {

    private:

        bitset<_coord_> possible;

        array<int, _block_> options;

    public:

        Grid & clear() {
            if (is_null()) return null();
            for_coord(c) possible[c] = true;
            for_block(b) options[b]  = _digit_;
            return *this;
        }

        Grid() {
            initialize();
            clear();
        }

        Grid & copy(const Grid &s) {
            if (is_null()) return null();
            for_coord(c) possible[c] = s.possible[c];
            for_block(b) options[b]  = s.options[b];
            return *this;
        }

        Grid(const Grid &s) {
            copy(s);
        }

        Grid & solve() {
            if (is_null()) return null();
            int min = INFINITY;
            block b;
            for_block(bb) if (options[bb] < min) min = options[b = bb];
            if (min == INFINITY) return *this;
            Grid g = *this;
            for_each(coords[b], itc) if (g.possible[*itc]) {
                if (!copy(g).assign(*itc).solve().is_null())
                    return *this;
            }
            return null();
        }

        Grid & read(string s) {
            if (is_null()) return null();
            auto itc = s.begin();
            for_digit(i) for_digit(j) {
                if (itc == s.end()) return *this;
                if ('1' <= *itc && *itc <= '9') {
                    digit d = *itc - '1';
                    if (assign(to_coord(i, j, d)).is_null()) return null();
                }
                itc++;
            }
            return *this;
        }

        Grid & read() {
            string s;
            cin >> s;
            return read(s);
        }

        string show() {
            if (is_null()) return "NULL\n";
            string s;
            for_digit(i) {
                for_digit(j) {
                    int cnt = 0;
                    digit d;
                    for_digit(dd) if (possible[to_coord(i, j, dd)]) { cnt++; d = dd; }
                    s += cnt == 1 ? '1' + (char)d : '.';
                }
                s += '\n';
            }
            return s;
        }

        void print() {
            cout << show() << endl;
        }

        bool is_null() {
            return this == NULL;
        }

        static Grid & null() {
            Grid *pg = NULL;
            return *pg;
        }

    private:

        Grid & assign(coord c) {
            if (is_null() || !possible[c]) return null();
            list<coord> cs;
            list<block> bs;
            for_each(others[c], itc) if (possible[*itc]) {
                cs.push_back(*itc);
                for_each(owners[*itc], itb) bs.push_back(*itb);
            }
            for_each(cs, itc) possible[*itc] = false;
            for_each(bs, itb) options[*itb]--;
            for_each(owners[c], itb) options[*itb] = DEFINED;
            for_each(bs, itb) if (check(*itb).is_null()) return null();
            return *this;
        }

        Grid & check(block b) {
            if (is_null()) return null();
            switch (options[b]) {
                case 0:
                    return null();
                case 1:
                    coord c;
                    for_each(coords[b], itc) if (possible[*itc]) c = *itc;
                    return assign(c);
                default:
                    return *this;
            }
        }

        static array<list<coord>, _block_> coords;
        static array<list<block>, _coord_> owners;
        static array<list<coord>, _coord_> others;

        static bool is_initialized;

        static void initialize() {
            if (is_initialized) return;
            for_digit(i) for_digit(j) for_digit(d) {
                coord c = to_coord(i, j, d);
                digit p = i / _box_ * _box_ + j / _box_;
                coords[to_block(GRD, i, j)].push_back(c);
                coords[to_block(ROW, i, d)].push_back(c);
                coords[to_block(COL, j, d)].push_back(c);
                coords[to_block(BOX, p, d)].push_back(c);
            }
            for_block(b) for_each(coords[b], itc) owners[*itc].push_back(b);
            for_coord(c) {
                set<coord> cs;
                for_each(owners[c], itb) for_each(coords[*itb], itc) cs.insert(*itc);
                cs.erase(c);
                for_each(cs, itc) others[c].push_back(*itc);
            }
            is_initialized = true;
        }
};

array<list<coord>, _block_> Grid::coords;
array<list<block>, _coord_> Grid::owners;
array<list<coord>, _coord_> Grid::others;

bool Grid::is_initialized = false;

int main() {
    Grid g;
    g.read().solve().print();
}
