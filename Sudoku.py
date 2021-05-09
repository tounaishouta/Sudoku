from __future__ import annotations
from typing import Optional
from enum import IntEnum


def main():
    while True:
        try:
            print(solve(input()))
        except EOFError:
            break


def solve(problem: str) -> str:
    try:
        return str(Sudoku().feed(problem).search())
    except NoSolutionException:
        return 'NO SOLUTION'


UNIT = 3
SIZE = UNIT * UNIT
DIGITS = '123456789'

Coord = int
Block = int


class State(IntEnum):
    TBD = 0
    TRUE = 1
    FALSE = 2


class View(IntEnum):
    GRD = 0
    ROW = 1
    COL = 2
    BOX = 3


def coord(n: int, i: int, j: int) -> Coord:
    return (n * SIZE + i) * SIZE + j


def block(v: int, p: int, q: int) -> Block:
    return (v * SIZE + p) * SIZE + q


COORD = SIZE * SIZE * SIZE
BLOCK = len(View) * SIZE * SIZE

DONE = SIZE + 1

relations: [(Coord, Block)] = [
    (coord(n, i, j), b)
    for n in range(SIZE)
    for i in range(SIZE)
    for j in range(SIZE)
    for b in [
        block(View.GRD, i, j),
        block(View.ROW, n, i),
        block(View.COL, n, j),
        block(View.BOX, n, i // UNIT * UNIT + j // UNIT),
    ]
]

parents: [[Block]] = [
    [b for (c, b) in relations if c == c0]
    for c0 in range(COORD)
]

children: [[Coord]] = [
    [c for (c, b) in relations if b == b0]
    for b0 in range(BLOCK)
]


class Sudoku:

    def __init__(self, other: Optional[Sudoku] = None):
        if other:
            self.state = other.state[:]
            self.count = other.count[:]
            self.queue = other.queue[:]
        else:
            self.state = [State.TBD] * COORD
            self.count = [SIZE] * BLOCK
            self.queue = []

    def __str__(self) -> str:
        res = ['.'] * (SIZE * SIZE)
        for n in range(SIZE):
            for i in range(SIZE):
                for j in range(SIZE):
                    if self.state[coord(n, i, j)] == State.TRUE:
                        res[i * SIZE + j] = DIGITS[n]
        return ''.join(res)

    def feed(self, problem: str) -> Sudoku:
        for ij, ch in enumerate(problem):
            if ch in DIGITS:
                n = DIGITS.index(ch)
                i, j = divmod(ij, SIZE)
                self.enqueue(coord(n, i, j))
        return self

    def enqueue(self, c: Coord) -> Sudoku:
        self.queue.append(c)
        return self

    def search(self) -> Sudoku:
        while self.queue:
            c0 = self.queue.pop()
            if self.state[c0] == State.TRUE:
                continue
            if self.state[c0] == State.FALSE:
                raise NoSolutionException()
            assert self.state[c0] == State.TBD
            self.state[c0] = State.TRUE
            for b1 in parents[c0]:
                assert self.count[b1] != DONE
                self.count[b1] = DONE
                for c2 in children[b1]:
                    if c2 == c0:
                        continue
                    if self.state[c2] == State.TRUE:
                        raise NoSolutionException()
                    if self.state[c2] == State.FALSE:
                        continue
                    assert self.state[c2] == State.TBD
                    self.state[c2] = State.FALSE
                    for b3 in parents[c2]:
                        if b3 == b1:
                            continue
                        assert self.count[b3] != DONE
                        self.count[b3] -= 1
                        if self.count[b3] == 0:
                            raise NoSolutionException()
                        if self.count[b3] == 1:
                            for c4 in children[b3]:
                                if self.state[c4] == State.TBD:
                                    self.enqueue(c4)
        m = min(self.count)
        if m == DONE:
            return self
        b = self.count.index(m)
        for c in children[b]:
            if self.state[c] == State.TBD:
                try:
                    return Sudoku(self).enqueue(c).search()
                except NoSolutionException:
                    pass
        raise NoSolutionException()


class NoSolutionException(Exception):
    pass


if __name__ == '__main__':
    main()
