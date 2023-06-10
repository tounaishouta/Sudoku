class NoSolutionException(Exception):
    pass


class Variable:
    def __init__(self, name):
        self.name = name
        self.constraints = []
        self.reset()

    def add_constraint(self, con):
        self.constraints.append(con)

    def reset(self):
        self.is_available = True
        self.is_adopted = False

    def save(self):
        return self.is_available, self.is_adopted

    def load(self, data):
        self.is_available, self.is_adopted = data

    def adopt(self):
        if self.is_adopted:
            return
        if not self.is_available:
            raise NoSolutionException
        self.is_adopted = True
        for con in self.constraints:
            con.adopt(self.name)

    def disable(self):
        if self.is_adopted:
            raise NoSolutionException
        if not self.is_available:
            return
        self.is_available = False
        for con in self.constraints:
            con.decrement()


class Constraint:
    def __init__(self, variables):
        self.variables = variables
        for var in self.variables:
            var.add_constraint(self)
        self.reset()

    def reset(self):
        self.adopted = None
        self.n_available = len(self.variables)

    def save(self):
        return self.adopted, self.n_available

    def load(self, data):
        self.adopted, self.n_available = data

    def adopt(self, var_name):
        if self.adopted == var_name:
            return
        if self.adopted is not None:
            raise NoSolutionException
        self.adopted = var_name
        for var in self.variables:
            if var.name != var_name:
                var.disable()

    def decrement(self):
        self.n_available -= 1
        if self.n_available == 0:
            raise NoSolutionException
        if self.n_available == 1:
            for var in self.variables:
                if var.is_available:
                    var.adopt()


class Solver:
    def __init__(self, var_names, var_groups):
        self.var_dict = {
            var_name: Variable(var_name)
            for var_name in var_names
        }
        self.variables = list(self.var_dict.values())
        self.constraints = [
            Constraint([self.var_dict[var_name] for var_name in var_group])
            for var_group in var_groups
        ]

    def reset(self):
        for var in self.variables:
            var.reset()
        for con in self.constraints:
            con.reset()

    def solve(self, hints):
        self.reset()
        for var_name in hints:
            self.adopt(var_name)
        self.search()
        for var in self.variables:
            if var.is_adopted:
                yield var.name

    def adopt(self, var_name):
        self.var_dict[var_name].adopt()

    def search(self):
        not_adopted = [con for con in self.constraints if con.adopted is None]
        if len(not_adopted) == 0:
            return
        con = min(not_adopted, key=lambda con: con.n_available)
        data = self.save()
        for var in con.variables:
            if not var.is_available:
                continue
            try:
                var.adopt()
                self.search()
                return
            except NoSolutionException:
                self.load(data)
        raise NoSolutionException

    def save(self):
        return (
            [var.save() for var in self.variables],
            [con.save() for con in self.constraints],
        )

    def load(self, data):
        var_data, con_data = data
        for var, d in zip(self.variables, var_data):
            var.load(d)
        for con, d in zip(self.constraints, con_data):
            con.load(d)


class SudokuSolver:
    def __init__(self):
        self.m = 3
        self.n = self.m ** 2
        self.digits = '123456789'
        assert len(self.digits) == self.n

        var_names = [
            (i, j, k)
            for i in self.indices()
            for j in self.indices()
            for k in self.indices()
        ]

        var_groups = [
            *[
                [(i, j, k) for k in self.indices()]
                for i in self.indices()
                for j in self.indices()
            ],
            *[
                [(i, j, k) for j in self.indices()]
                for i in self.indices()
                for k in self.indices()
            ],
            *[
                [(i, j, k) for i in self.indices()]
                for j in self.indices()
                for k in self.indices()
            ],
            *[
                [(*self.box(p, q), k) for q in self.indices()]
                for p in self.indices()
                for k in self.indices()
            ]
        ]

        self.solver = Solver(var_names, var_groups)

    def indices(self):
        return range(self.n)

    def box(self, p, q):
        pi, pj = divmod(p, self.m)
        qi, qj = divmod(q, self.m)
        return (pi * self.m + qi, pj * self.m + qj)

    def solve(self, problem):
        hints = self.extract_hints(problem)
        solution = self.solver.solve(hints)
        answer = self.string_from(solution)
        return answer

    def extract_hints(self, problem):
        coords = [(i, j) for i in self.indices() for j in self.indices()]
        for (i, j), ch in zip(coords, problem):
            try:
                k = self.digits.index(ch)
                yield (i, j, k)
            except ValueError:
                pass

    def string_from(self, hints, sep_col='', sep_row=''):
        mat = [['.' for j in self.indices()] for i in self.indices()]
        for (i, j, k) in hints:
            mat[i][j] = self.digits[k]
        return sep_row.join(sep_col.join(row) for row in mat)


if __name__ == '__main__':
    solver = SudokuSolver()
    while True:
        try:
            problem = input()
            answer = solver.solve(problem)
            print(answer)
        except EOFError:
            break
