def main():
    s = Sudoku()
    while True:
        try:
            print(s.solve(input()))
        except EOFError:
            break


UNIT = 3
SIZE = UNIT * UNIT
DIGITS = '123456789'

COORDS = SIZE * SIZE * SIZE
BLOCKS = 4 * SIZE * SIZE

OPEN = 0
FIXED = 1
BANNED = 2

DONE = SIZE + 1

INITIAL_STATE = [OPEN for _ in range(COORDS)]
INITIAL_COUNT = [SIZE for _ in range(BLOCKS)]


def coord(i: int, j: int, k: int) -> int:
    return (i * SIZE + j) * SIZE + k


b2cs: list[list[int]] = [
    *[
        [coord(i, j, k) for k in range(SIZE)]
        for i in range(SIZE)
        for j in range(SIZE)
    ],
    *[
        [coord(i, j, k) for j in range(SIZE)]
        for i in range(SIZE)
        for k in range(SIZE)
    ],
    *[
        [coord(i, j, k) for i in range(SIZE)]
        for j in range(SIZE)
        for k in range(SIZE)
    ],
    *[
        [coord(ii * UNIT + i, jj * UNIT + j, k)
         for i in range(UNIT) for j in range(UNIT)]
        for ii in range(UNIT)
        for jj in range(UNIT)
        for k in range(SIZE)
    ],
]


c2bs: list[list[int]] = [[] for _ in range(COORDS)]
for b, cs in enumerate(b2cs):
    for c in cs:
        c2bs[c].append(b)


class NoSolutionException(Exception):
    pass


class Sudoku:
    def __init__(self) -> None:
        self.state = [*INITIAL_STATE]
        self.count = [*INITIAL_COUNT]
        self.queue = []
        self.cache = None

    def __str__(self) -> str:
        mat = [['.' for _ in range(SIZE)] for _ in range(SIZE)]
        for i in range(SIZE):
            for j in range(SIZE):
                for k in range(SIZE):
                    if self.state[coord(i, j, k)] == FIXED:
                        mat[i][j] = DIGITS[k]
        return ''.join(''.join(row) for row in mat)

    def solve(self, problem: str) -> str:
        self.reset()
        self.feed(problem)
        try:
            return str(self.search())
        except NoSolutionException:
            return 'NO SOLUTION'

    def reset(self) -> None:
        self.state[:] = INITIAL_STATE
        self.count[:] = INITIAL_COUNT
        self.queue.clear()

    def load(self, s: 'Sudoku') -> None:
        self.state[:] = s.state
        self.count[:] = s.count
        self.queue.clear()

    def feed(self, problem: str) -> None:
        for ij, ch in enumerate(problem[:(SIZE * SIZE)]):
            if ch in DIGITS:
                i, j = divmod(ij, SIZE)
                k = DIGITS.index(ch)
                self.queue.append(coord(i, j, k))

    def search(self) -> 'Sudoku':
        while self.queue:
            c0 = self.queue.pop()
            if self.state[c0] == FIXED:
                continue
            if self.state[c0] == BANNED:
                raise NoSolutionException()
            self.state[c0] = FIXED
            for b1 in c2bs[c0]:
                self.count[b1] = DONE
                for c2 in b2cs[b1]:
                    if c2 == c0:
                        continue
                    if self.state[c2] == BANNED:
                        continue
                    if self.state[c2] == FIXED:
                        assert False
                    self.state[c2] = BANNED
                    for b3 in c2bs[c2]:
                        if b3 == b1:
                            continue
                        if self.count[b3] == DONE:
                            assert False
                        self.count[b3] -= 1
                        if self.count[b3] == 0:
                            raise NoSolutionException()
                        if self.count[b3] == 1:
                            for c4 in b2cs[b3]:
                                if self.state[c4] == OPEN:
                                    self.queue.append(c4)

        bmin = None
        mincnt = DONE
        for b, cnt in enumerate(self.count):
            if cnt < mincnt:
                bmin = b
                mincnt = cnt

        if mincnt == DONE:
            return self

        if self.cache is None:
            self.cache = Sudoku()
        for c in b2cs[bmin]:
            if self.state[c] == OPEN:
                self.cache.load(self)
                self.cache.queue.append(c)
                try:
                    return self.cache.search()
                except NoSolutionException:
                    pass

        raise NoSolutionException()


if __name__ == '__main__':
    main()
