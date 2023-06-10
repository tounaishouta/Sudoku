package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

func main() {
	s := NewSolver(3, 3, []rune("123456789"))
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		fmt.Println(s.Solve(scanner.Text()))
	}
}

func (s *Solver) Solve(problem string) string {
	b := s.newBoard()
	s.feed(b, problem)
	if b, ok := s.search(b); ok {
		return s.sprint(b)
	} else {
		return "NO SOLUTION"
	}
}

type (
	view   int
	coord  int
	group  int
	Solver struct {
		l      int
		m      int
		n      int
		nCoord int
		nGroup int
		groups [][]group
		coords [][]coord
		digits []rune
	}
)

const (
	grd view = iota
	row
	col
	box
	nview
)

func NewSolver(l int, m int, digits []rune) *Solver {
	n := l * m
	if len(digits) != n {
		panic("Invalid arguments")
	}
	s := &Solver{
		l:      l,
		m:      m,
		n:      n,
		nCoord: n * n * n,
		nGroup: int(nview) * n * n,
		digits: digits,
	}
	s.groups = s.makeGroups()
	s.coords = s.makeCoords()
	return s
}

func (s *Solver) coord(i int, j int, k int) coord {
	return coord((i*s.n+j)*s.n + k)
}

func (s *Solver) group(v view, x int, y int) group {
	return group((int(v)*s.n+x)*s.n + y)
}

func (s *Solver) makeGroups() [][]group {
	groups := make([][]group, s.nCoord)
	for i := 0; i < s.n; i++ {
		for j := 0; j < s.n; j++ {
			p := i/s.l*s.l + j/s.m
			for k := 0; k < s.n; k++ {
				groups[s.coord(i, j, k)] = []group{
					s.group(grd, i, j),
					s.group(row, i, k),
					s.group(col, j, k),
					s.group(box, p, k),
				}
			}
		}
	}
	return groups
}

func (s *Solver) makeCoords() [][]coord {
	coords := make([][]coord, s.nGroup)
	for g := range coords {
		coords[g] = make([]coord, 0, s.n)
	}
	for c, gs := range s.groups {
		for _, g := range gs {
			coords[g] = append(coords[g], coord(c))
		}
	}
	return coords
}

type (
	state int
	board struct {
		state []state
		count []int
		queue []coord
	}
)

const (
	open state = iota
	fixed
	banned
	done int = math.MaxInt
)

func (s *Solver) newBoard() *board {
	state := make([]state, s.nCoord)
	for c := range state {
		state[c] = open
	}
	count := make([]int, s.nGroup)
	for g := range count {
		count[g] = s.n
	}
	queue := make([]coord, 0)
	return &board{
		state: state,
		count: count,
		queue: queue,
	}
}

func (s *Solver) copy(b *board) *board {
	state := make([]state, s.nCoord)
	for c := range state {
		state[c] = b.state[c]
	}
	count := make([]int, s.nGroup)
	for g := range count {
		count[g] = b.count[g]
	}
	queue := make([]coord, 0)
	return &board{
		state: state,
		count: count,
		queue: queue,
	}
}

func (s *Solver) feed(b *board, problem string) {
	runes := []rune(problem)
	for i := 0; i < s.n; i++ {
		for j := 0; j < s.n; j++ {
			ij := i*s.n + j
			if ij < len(runes) {
				for k := 0; k < s.n; k++ {
					if runes[ij] == s.digits[k] {
						b.queue = append(b.queue, s.coord(i, j, k))
					}
				}
			}
		}
	}
}

func (s *Solver) sprint(b *board) string {
	runes := make([]rune, s.n*s.n)
	for ij := range runes {
		runes[ij] = '.'
	}
	for i := 0; i < s.n; i++ {
		for j := 0; j < s.n; j++ {
			ij := i*s.n + j
			for k := 0; k < s.n; k++ {
				if b.state[s.coord(i, j, k)] == fixed {
					runes[ij] = s.digits[k]
				}
			}
		}
	}
	return string(runes)
}

func (s *Solver) search(b *board) (*board, bool) {
	for len(b.queue) > 0 {
		c := b.queue[0]
		b.queue = b.queue[1:]
		if ok := s.assign(b, c); !ok {
			return nil, false
		}
	}
	gmin := -1
	mincnt := done
	for g, cnt := range b.count {
		if cnt < mincnt {
			gmin = g
			mincnt = cnt
		}
	}
	if mincnt == done {
		return b, true
	}
	for _, c := range s.coords[gmin] {
		if b.state[c] == open {
			b := s.copy(b)
			if ok := s.assign(b, c); ok {
				if b, ok := s.search(b); ok {
					return b, true
				}
			}
		}
	}
	return nil, false
}

func (s *Solver) assign(b *board, c0 coord) bool {
	if b.state[c0] == fixed {
		return true
	}
	if b.state[c0] == banned {
		return false
	}
	b.state[c0] = fixed
	for _, g1 := range s.groups[c0] {
		if b.count[g1] == done {
			panic("Something wrong")
		}
		b.count[g1] = done
		for _, c2 := range s.coords[g1] {
			if c2 == c0 {
				continue
			}
			if b.state[c2] == banned {
				continue
			}
			if b.state[c2] == fixed {
				panic("Something wrong")
			}
			b.state[c2] = banned
			for _, g3 := range s.groups[c2] {
				if b.count[g3] == done {
					continue
				}
				b.count[g3]--
				if b.count[g3] == 0 {
					return false
				}
				if b.count[g3] == 1 {
					for _, c4 := range s.coords[g3] {
						if b.state[c4] == open {
							b.queue = append(b.queue, c4)
						}
					}
				}
			}
		}
	}
	return true
}
