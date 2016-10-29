import fileinput

GRD   = 0
ROW   = 1
COL   = 2
BOX   = 3
VIEW  = 4
UNIT  = 3
SIZE  = UNIT * UNIT
BLOCK = VIEW * SIZE * SIZE
COORD = SIZE * SIZE * SIZE

DEFINED = 0xDEF

FEASIBLE = 0
BANNED   = 1
FIXED    = 2

DIGITS = "123456789"

def block(v, p, q):
    return (v * SIZE + p) * SIZE + q

def coord(i, j, k):
    return (i * SIZE + j) * SIZE + k

parents = [None] * COORD
for i in range(SIZE):
    for j in range(SIZE):
        p = i // UNIT * UNIT + j // UNIT
        for k in range(SIZE):
            parents[coord(i, j, k)] = [
                    block(GRD, i, j),
                    block(ROW, i, k),
                    block(COL, j, k),
                    block(BOX, p, k),
                    ]

children = [None] * BLOCK
for b in range(BLOCK):
    children[b] = []
for c in range(COORD):
    for b in parents[c]:
        children[b].append(c)

class NoSolutionException(Exception):
    pass

class Sudoku:

    def __init__(self, other = None):
        if other is None:
            self.bstate = [SIZE] * BLOCK
            self.cstate = [FEASIBLE] * COORD
        else:
            self.bstate = other.bstate[:]
            self.cstate = other.cstate[:]
        self.queue = []

    def __str__(self):
        result = ["."] * (SIZE * SIZE)
        for i in range(SIZE):
            for j in range(SIZE):
                for k in range(SIZE):
                    if self.cstate[coord(i, j, k)] == FIXED:
                        result[i * SIZE + j] = DIGITS[k]
        return "".join(result)

    def assign(self, c):
        self.queue.append(c)
        return self

    def read(self, problem):
        for ij in range(min(SIZE * SIZE, len(problem))):
            k = DIGITS.find(problem[ij])
            if k != -1:
                self.assign(coord(ij // SIZE, ij % SIZE, k))
        return self

    def search(self):

        while len(self.queue) > 0:
            c0 = self.queue.pop()
            if self.cstate[c0] == FIXED:
                continue
            if self.cstate[c0] == BANNED:
                raise NoSolutionException()
            self.cstate[c0] = FIXED
            for b1 in parents[c0]:
                self.bstate[b1] = DEFINED
                for c2 in children[b1]:
                    if c2 != c0 and self.cstate[c2] == FEASIBLE:
                        self.cstate[c2] = BANNED
                        for b3 in parents[c2]:
                            if b3 != b1:
                                self.bstate[b3] -= 1
                                if self.bstate[b3] == 0:
                                    raise NoSolutionException()
                                if self.bstate[b3] == 1:
                                    for c4 in children[b3]:
                                        if self.cstate[c4] == FEASIBLE:
                                            self.assign(c4)

        m = min(self.bstate)
        if m == DEFINED:
            return self

        b = self.bstate.index(m)
        for c in children[b]:
            if self.cstate[c] == FEASIBLE:
                try:
                    return Sudoku(self).assign(c).search()
                except NoSolutionException:
                    pass

        raise NoSolutionException()

def solve(problem):
    try:
        return str(Sudoku().read(problem).search())
    except NoSolutionException:
        return "NO SOLUTION"

for problem in fileinput.input():
    print(solve(problem))
