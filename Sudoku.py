import fileinput

UNIT  = 3
SIZE  = UNIT * UNIT
GRD   = 0
ROW   = 1
COL   = 2
BOX   = 3
VIEW  = 4
COORD = SIZE * SIZE * SIZE
BLOCK = VIEW * SIZE * SIZE

DEFINED = 0xDEF

DIGITS = "123456789"

def coord(i, j, k):
    return (i * SIZE + j) * SIZE + k

def block(v, p, q):
    return (v * SIZE + p) * SIZE + q

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

class NoSolutionException(Exception):
    def __str__(self):
        return "NO SOLUTION"

class Sudoku:

    def __init__(self, other = None):
        if other == None:
            self.admit = [True] * COORD
            self.count = [SIZE] * BLOCK
        else:
            self.admit = other.admit[:]
            self.count = other.count[:]

    def __str__(self):
        result = [None] * (SIZE * SIZE);
        for i in xrange(SIZE):
            for j in xrange(SIZE):
                ks = [ k for k in xrange(SIZE) if self.admit[coord(i, j, k)] ]
                if len(ks) == 1:
                    result[i * SIZE + j] = DIGITS[ks[0]]
                else:
                    result[i * SIZE + j] = '.'
        return "".join(result)

    def search(self):
        m = min(self.count)
        if m == DEFINED:
            return self
        b = self.count.index(m)
        for c in children[b]:
            if self.admit[c]:
                try:
                    return Sudoku(self).assign(c).search()
                except NoSolutionException:
                    pass
        raise NoSolutionException()

    def read(self, problem):
        for ij, char in enumerate(problem):
            k = DIGITS.find(char)
            if k != -1:
                self.assign(coord(ij / SIZE, ij % SIZE, k))
        return self

    def assign(self, c):
        queue = [c]
        while len(queue) > 0:
            c0 = queue.pop()
            if not self.admit[c0]:
                raise NoSolutionException()
            for b1 in parents[c0]:
                self.count[b1] = DEFINED
                for c2 in children[b1]:
                    if c2 != c0 and self.admit[c2]:
                        self.admit[c2] = False
                        for b3 in parents[c2]:
                            if b3 != b1:
                                self.count[b3] -= 1
                                if self.count[b3] == 0:
                                    raise NoSolutionException()
                                if self.count[b3] == 1:
                                    for c4 in children[b3]:
                                        if self.admit[c4]:
                                            queue.append(c4)
        return self

def solve(problem):
    try:
        return str(Sudoku().read(problem).search())
    except NoSolutionException as e:
        return str(e)

for problem in fileinput.input():
    print(solve(problem))
