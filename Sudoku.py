# for python2

import fileinput

UNIT    = 3
SIZE    = UNIT * UNIT
COORD   = SIZE * SIZE * SIZE
GRD     = 0
ROW     = 1
COL     = 2
BOX     = 3
VIEW    = 4
BLOCK   = VIEW * SIZE * SIZE
DEFINED = 0xDEF
DIGITS  = "123456789"

def coord(i, j, k):
    return (i * SIZE + j) * SIZE + k

def block(v, i, j):
    return (v * SIZE + i) * SIZE + j

parents = [None] * COORD
for i in xrange(SIZE):
    for j in xrange(SIZE):
        p = i / UNIT * UNIT + j / UNIT
        for k in xrange(SIZE):
            parents[coord(i, j, k)] = [
                    block(GRD, i, j),
                    block(ROW, i, k),
                    block(COL, j, k),
                    block(BOX, p, k),
                    ]

children = [None] * BLOCK
for b in xrange(BLOCK):
    children[b] = []
for c in xrange(COORD):
    for b in parents[c]:
        children[b].append(c)

siblings = [None] * COORD
for c in xrange(COORD):
    ccs = set()
    for b in parents[c]:
        for cc in children[b]:
            ccs.add(cc)
    ccs.discard(c)
    siblings[c] = list(ccs)

class NoSolutionError(Exception):
    def __str__(self):
        return "NO SOLUTION"

class Sudoku:

    def __init__(self):
        self.admits = [True] * COORD
        self.counts = [SIZE] * BLOCK

    def __str__(self):
        res = ['x'] * (SIZE * SIZE)
        for i in xrange(SIZE):
            for j in xrange(SIZE):
                ij = i * SIZE + j
                cnt = 0
                for k in xrange(SIZE):
                    if self.admits[coord(i, j, k)]:
                        res[ij] = DIGITS[k]
                        cnt += 1
                if cnt > 1:
                    res[ij] = '?'
        return ''.join(res)

    def read(self, problem):
        for ij, char in enumerate(problem):
            k = DIGITS.find(char)
            if k != -1:
                i = ij / SIZE
                j = ij % SIZE
                self.assign(coord(i, j, k))

    def assign(self, c):
        if not self.admits[c]:
            raise NoSolutionError()
        bs = set()
        for cc in siblings[c]:
            if self.admits[cc]:
                self.admits[cc] = False
                for b in parents[cc]:
                    self.counts[b] -= 1
                    bs.add(b)
        for b in parents[c]:
            self.counts[b] = DEFINED
            bs.discard(b)
        for b in bs:
            self.check(b)

    def check(self, b):
        if self.counts[b] == 0:
            raise NoSolutionError()
        elif self.counts[b] == 1:
            for c in children[b]:
                if self.admits[c]:
                    self.assign(c)

    def search(self):
        m = min(self.counts)
        if m == DEFINED:
            return self
        b = self.counts.index(m)
        for c in children[b]:
            if self.admits[c]:
                try:
                    s = self.copy()
                    s.assign(c)
                    return s.search()
                except NoSolutionError:
                    pass
        raise NoSolutionError()

    def copy(self):
        res = Sudoku()
        for c in xrange(COORD):
            res.admits[c] = self.admits[c]
        for b in xrange(BLOCK):
            res.counts[b] = self.counts[b]
        return res

def solve(problem):
    try:
        s = Sudoku()
        s.read(problem)
        return s.search()
    except NoSolutionError as e:
        return str(e)

for problem in fileinput.input():
    print(solve(problem))
