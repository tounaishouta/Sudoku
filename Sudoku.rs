use std::collections::VecDeque;

fn main() {
    use std::io::BufRead;
    let rule = Rule::new();
    let stdin = std::io::stdin();
    for problem in stdin.lock().lines().filter_map(|r| r.ok()) {
        println!("{}", Sudoku::solve(problem, &rule));
    }
}

struct Rule {
    size: usize,
    digits: String,
    n_coord: usize,
    n_block: usize,
    parents: Vec<Vec<Block>>,
    children: Vec<Vec<Coord>>,
}

type Block = usize;
type Coord = usize;

impl Rule {
    fn new() -> Self {
        let unit = 3;
        let size = unit * unit;
        let digits = "123456789".to_string();
        assert_eq!(size, digits.len());
        let n_coord = size * size * size;
        let n_block = 4 * size * size;
        let mut parents = Vec::with_capacity(n_coord);
        for i in 0..size {
            for j in 0..size {
                let p = i / unit * unit + j / unit;
                for k in 0..size {
                    parents.push(vec![
                        (0 * size + i) * size + j,
                        (1 * size + i) * size + k,
                        (2 * size + j) * size + k,
                        (3 * size + p) * size + k,
                    ]);
                }
            }
        }
        let mut children = vec![Vec::with_capacity(size); n_block];
        for c in 0..n_coord {
            for &b in &parents[c] {
                children[b].push(c);
            }
        }
        Rule {
            size,
            digits,
            n_coord,
            n_block,
            parents,
            children,
        }
    }
}

#[derive(Clone)]
struct Sudoku<'a> {
    state: Vec<State>,
    count: Vec<Count>,
    queue: VecDeque<Coord>,
    rule: &'a Rule,
}

#[derive(Clone, PartialEq)]
enum State {
    Vague,
    False,
    True,
}

type Count = usize;

const COUNT_DONE: Count = std::usize::MAX;

impl<'a> Sudoku<'a> {
    fn solve(problem: String, rule: &'a Rule) -> String {
        let mut s = Sudoku::new(rule);
        s.feed(problem);
        if let Some(s) = s.search() {
            s.display()
        } else {
            "NO SOLUTION".to_string()
        }
    }
    fn new(rule: &'a Rule) -> Self {
        Sudoku {
            state: vec![State::Vague; rule.n_coord],
            count: vec![rule.size; rule.n_block],
            queue: VecDeque::new(),
            rule,
        }
    }
    fn feed(&mut self, problem: String) {
        let size = self.rule.size;
        for (ij, ch) in problem.chars().take(size * size).enumerate() {
            let i = ij / size;
            let j = ij % size;
            if let Some(k) = self.rule.digits.find(ch) {
                self.queue.push_back((i * size + j) * size + k);
            }
        }
    }
    fn display(&self) -> String {
        let size = self.rule.size;
        let mut res = String::with_capacity(size * size);
        for i in 0..size {
            for j in 0..size {
                res.push(
                    (0..size)
                        .find(|&k| self.state[(i * size + j) * size + k] == State::True)
                        .map_or('.', |k| self.rule.digits.chars().nth(k).unwrap()),
                );
            }
        }
        res
    }
    fn search(mut self) -> Option<Self> {
        while let Some(c) = self.queue.pop_front() {
            if !self.assign(c) {
                return None;
            }
        }
        let mut b_min = 0;
        let mut min = self.count[b_min];
        for b in 1..self.rule.n_block {
            if self.count[b] < min {
                b_min = b;
                min = self.count[b_min];
            }
        }
        if min == COUNT_DONE {
            return Some(self);
        }
        self.rule.children[b_min].iter().find_map(|&c| {
            let mut s = self.clone();
            s.queue.push_back(c);
            s.search()
        })
    }
    fn assign(&mut self, c: Coord) -> bool {
        match self.state[c] {
            State::Vague => {
                self.state[c] = State::True;
                self.rule.parents[c].iter().all(|&b| self.mark(b))
            }
            State::False => false,
            State::True => true,
        }
    }
    fn mark(&mut self, b: Block) -> bool {
        self.count[b] = COUNT_DONE;
        self.rule.children[b].iter().all(|&c| self.ban(c))
    }
    fn ban(&mut self, c: Coord) -> bool {
        match self.state[c] {
            State::Vague => {
                self.state[c] = State::False;
                self.rule.parents[c].iter().all(|&b| self.countdown(b))
            }
            _ => true,
        }
    }
    fn countdown(&mut self, b: Block) -> bool {
        if self.count[b] == COUNT_DONE {
            return true;
        }
        self.count[b] -= 1;
        if self.count[b] == 0 {
            return false;
        }
        if self.count[b] == 1 {
            for &c in &self.rule.children[b] {
                if self.state[c] == State::Vague {
                    self.queue.push_back(c);
                }
            }
        }
        return true;
    }
}
