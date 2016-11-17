import fileinput

UNIT  = 3
VIEW  = 4
SIZE  = UNIT * UNIT
COORD = SIZE * SIZE * SIZE
BLOCK = VIEW * SIZE * SIZE

GRD = 0
ROW = 1
COL = 2
BOX = 3

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

    def enqueue(self, c):
        self.queue.append(c)
        return self

    def read(self, problem):
        l = len(problem);
        for i in range(SIZE):
            for j in range(SIZE):
                if i * SIZE + j < l:
                    k = DIGITS.find(problem[i * SIZE + j])
                    if k != -1:
                        self.enqueue(coord(i, j, k))
        return self

    def search(self):

        while len(self.queue) > 0:
            c = self.queue.pop()
            if self.state[c] == OPEN:
                self.fix(c)
            elif self.state[c] == FIXED:
                continue
            elif self.state[c] == BANNED:
                raise NoSolutionException()

        m = min(self.count)
        if m == DONE:
            return self

        b = self.count.index(m)
        for c in children[b]:
            if self.state[c] == OPEN:
                try:
                    return Sudoku(self).enqueue(c).search()
                except NoSolutionException:
                    pass

        raise NoSolutionException()

    def fix(self, c):
        self.state[c] = FIXED
        for b in parents[c]:
            self.mark(b)

    def mark(self, b):
        self.count[b] = DONE
        for c in children[b]:
            if self.state[c] == OPEN:
                self.ban(c)

    def ban(self, c):
        self.state[c] = BANNED
        for b in parents[c]:
            if self.count[b] != DONE:
                self.countdown(b)

    def countdown(self, b):
        self.count[b] -= 1
        if self.count[b] == 0:
            raise NoSolutionException
        if self.count[b] == 1:
            for c in children[b]:
                if self.state[c] == OPEN:
                    self.enqueue(c)

def solve(problem):
    try:
        return str(Sudoku().read(problem).search())
    except NoSolutionException:
        return "NO SOLUTION"

for problem in fileinput.input():
    print(solve(problem))
