import fileinput

UNIT  = 3
GRD   = 0
ROW   = 1
COL   = 2
BOX   = 3
VIEW  = 4
SIZE  = UNIT * UNIT
COORD = SIZE * SIZE * SIZE
BLOCK = VIEW * SIZE * SIZE

OPEN   = 0
FIXED  = 1
BANNED = 2

DONE = SIZE + 1

DIGITS = "123456789"

def coord(i, j, k):
    return (i * SIZE + j) * SIZE + k

def block(v, p, q):
    return (v * SIZE + p) * SIZE + q

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
            self.state = [OPEN] * COORD
            self.count = [SIZE] * BLOCK
        else:
            self.state = other.state[:]
            self.count = other.count[:]
        self.queue = []

    def __str__(self):
        result = ["."] * (SIZE * SIZE)
        for i in range(SIZE):
            for j in range(SIZE):
                for k in range(SIZE):
                    if self.state[coord(i, j, k)] == FIXED:
                        result[i * SIZE + j] = DIGITS[k]
        return "".join(result)

    def fix(self, c):
        self.queue.append(c)
        return self

    def read(self, problem):
        l = len(problem);
        for i in range(SIZE):
            for j in range(SIZE):
                if i * SIZE + j < l:
                    k = DIGITS.find(problem[i * SIZE + j])
                    if k != -1:
                        self.fix(coord(i, j, k))
        return self

    def search(self):

        while len(self.queue) > 0:
            c0 = self.queue.pop()
            if self.state[c0] == FIXED:
                continue
            if self.state[c0] == BANNED:
                raise NoSolutionException()
            self.state[c0] = FIXED
            for b1 in parents[c0]:
                self.count[b1] = DONE
                for c2 in children[b1]:
                    if c2 != c0 and self.state[c2] == OPEN:
                        self.state[c2] = BANNED
                        for b3 in parents[c2]:
                            if b3 != b1:
                                self.count[b3] -= 1
                                if self.count[b3] == 0:
                                    raise NoSolutionException()
                                if self.count[b3] == 1:
                                    for c4 in children[b3]:
                                        if self.state[c4] == OPEN:
                                            self.fix(c4)

        m = min(self.count)
        if m == DONE:
            return self

        b = self.count.index(m)
        for c in children[b]:
            if self.state[c] == OPEN:
                try:
                    return Sudoku(self).fix(c).search()
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
