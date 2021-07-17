def main():
    while True:
        try:
            print(Sudoku.solve(input()))
        except EOFError:
            break


class NoSolutionException(Exception):
    pass


class Sudoku:

    @classmethod
    def solve(cls, hints):
        for s in cls().feed(hints).search():
            return str(s)
        return 'NO SOLUTION'

    @classmethod
    def initialize(cls):
        if hasattr(cls, 'initialized'):
            return

        cls.unit = 3
        cls.size = cls.unit * cls.unit
        cls.digits = '123456789'
        assert len(cls.digits) == cls.size

        cls.coords = cls.size * cls.size * cls.size
        cls.groups = 4 * cls.size * cls.size

        cls.parents = [None for _ in range(cls.coords)]
        for i in range(cls.size):
            for j in range(cls.size):
                k = i // cls.unit * cls.unit + j // cls.unit
                for n in range(cls.size):
                    cls.parents[cls.coord(i, j, n)] = [
                        cls.group(0, i, j),
                        cls.group(1, i, n),
                        cls.group(2, j, n),
                        cls.group(3, k, n),
                    ]

        cls.children = [[] for _ in range(cls.groups)]
        for c in range(cls.coords):
            for g in cls.parents[c]:
                cls.children[g].append(c)

        cls.undefined = 0
        cls.true = 1
        cls.false = 2

        cls.defined = cls.size + 1

        cls.initialized = True

    @classmethod
    def coord(cls, i, j, n):
        return (i * cls.size + j) * cls.size + n

    @classmethod
    def group(cls, v, p, q):
        return (v * cls.size + p) * cls.size + q

    def __init__(self, other=None):
        self.initialize()
        if other is None:
            self.state = [self.undefined] * self.coords
            self.count = [self.size] * self.groups
            self.queue = []
        else:
            self.state = other.state[:]
            self.count = other.count[:]
            self.queue = other.queue[:]

    def enqueue(self, c):
        self.queue.append(c)
        return self

    def feed(self, hints):
        ijs = [(i, j) for i in range(self.size) for j in range(self.size)]
        for (i, j), ch in zip(ijs, hints):
            if ch in self.digits:
                n = self.digits.index(ch)
                self.queue.append(self.coord(i, j, n))
        return self

    def search(self):

        while self.queue:
            self.assign(self.queue.pop())

        m = min(self.count)
        if m == self.defined:
            yield self
        else:
            b = self.count.index(m)
            for c in self.children[b]:
                if self.state[c] == self.undefined:
                    try:
                        yield from self.clone().enqueue(c).search()
                    except NoSolutionException:
                        pass

    def assign(self, c):
        if self.state[c] == self.true:
            return
        if self.state[c] == self.false:
            raise NoSolutionException
        assert self.state[c] == self.undefined
        self.state[c] = self.true
        for b in self.parents[c]:
            self.close(b)

    def close(self, b):
        assert self.count[b] != self.defined
        assert self.count[b] > 0
        self.count[b] = self.defined
        for c in self.children[b]:
            self.ban(c)

    def ban(self, c):
        if self.state[c] == self.true:
            return
        if self.state[c] == self.false:
            return
        assert self.state[c] == self.undefined
        self.state[c] = self.false
        for b in self.parents[c]:
            self.countdown(b)

    def countdown(self, b):
        if self.count[b] == self.defined:
            return
        self.count[b] -= 1
        if self.count[b] == 0:
            raise NoSolutionException
        if self.count[b] == 1:
            for c in self.children[b]:
                if self.state[c] == self.undefined:
                    self.enqueue(c)

    def clone(self):
        return self.__class__(self)

    def __str__(self):
        matrix = [['.' for _ in range(self.size)] for _ in range(self.size)]
        for i in range(self.size):
            for j in range(self.size):
                for n in range(self.size):
                    if self.state[self.coord(i, j, n)] == self.true:
                        matrix[i][j] = self.digits[n]
        return ''.join(''.join(row) for row in matrix)


if __name__ == '__main__':
    main()
