package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		fmt.Println(solve(scanner.Text()))
	}
}

func solve(input string) string {
	if s, ok := newSudoku().read(input).search(); ok {
		return s.String()
	} else {
		return "NO SOLUTION"
	}
}

type (
	View   int
	Coord  int
	Block  int
	State  int
	Count  int
	Sudoku struct {
		state [nCoord]State
		count [nBlock]Count
		queue []Coord
	}
)

const (
	unit   = 3
	size   = unit * unit
	nCoord = size * size * size
	nBlock = 4 * size * size
)

const (
	grd View = iota
	row
	col
	box
)

const (
	open State = iota
	fixed
	banned
)

const (
	done Count = size + 1
)

var (
	digits = []rune("123456789")

	parents  [nCoord][]Block
	children [nBlock][]Coord
)

func coord(i, j, k int) Coord {
	return Coord((i*size+j)*size + k)
}

func block(v View, p, q int) Block {
	return Block((int(v)*size+p)*size + q)
}

func init() {
	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			p := i/unit*unit + j/unit
			for k := 0; k < size; k++ {
				parents[coord(i, j, k)] = []Block{
					block(grd, i, j),
					block(row, i, k),
					block(col, j, k),
					block(box, p, k),
				}
			}
		}
	}
	for c, bs := range parents {
		for _, b := range bs {
			children[b] = append(children[b], Coord(c))
		}
	}
}

func newSudoku() *Sudoku {
	s := new(Sudoku)
	for c := range s.state {
		s.state[c] = open
	}
	for b := range s.count {
		s.count[b] = Count(size)
	}
	return s
}

func (s *Sudoku) clone() *Sudoku {
	ss := new(Sudoku)
	for c := range s.state {
		ss.state[c] = s.state[c]
	}
	for b := range s.count {
		ss.count[b] = s.count[b]
	}
	return ss
}

func (s *Sudoku) enqueue(c Coord) *Sudoku {
	s.queue = append(s.queue, c)
	return s
}

func (s *Sudoku) read(input string) *Sudoku {
	array := []rune(input)
	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			if i*size+j < len(array) {
				r := array[i*size+j]
				for k := 0; k < size; k++ {
					if r == digits[k] {
						s.enqueue(coord(i, j, k))
					}
				}
			}
		}
	}
	return s
}

func (s *Sudoku) String() string {
	result := make([]rune, size*size)
	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			result[i*size+j] = '.'
			for k := 0; k < size; k++ {
				if s.state[coord(i, j, k)] == fixed {
					result[i*size+j] = digits[k]
				}
			}
		}
	}
	return string(result)
}

func (s *Sudoku) search() (*Sudoku, bool) {

	for len(s.queue) > 0 {
		c := s.queue[0]
		s.queue = s.queue[1:]
		switch s.state[c] {
		case open:
			if ok := s.fix(c); !ok {
				return nil, false
			}
		case fixed:
			continue
		case banned:
			return nil, false
		}
	}

	min, bmin := done, -1
	for b, cnt := range s.count {
		if cnt < min {
			min = cnt
			bmin = b
		}
	}

	if min == done {
		return s, true
	}

	for _, c := range children[bmin] {
		if s.state[c] == open {
			if ss, ok := s.clone().enqueue(c).search(); ok {
				return ss, true
			}
		}
	}

	return nil, false
}

func (s *Sudoku) fix(c Coord) (ok bool) {
	defer func() {
		if err := recover(); err != nil {
			ok = false
		}
	}()
	s.state[c] = fixed
	for _, b := range parents[c] {
		s.markDone(b)
	}
	return true
}

func (s *Sudoku) markDone(b Block) {
	s.count[b] = done
	for _, c := range children[b] {
		if s.state[c] == open {
			s.ban(c)
		}
	}
}

func (s *Sudoku) ban(c Coord) {
	s.state[c] = banned
	for _, b := range parents[c] {
		if s.count[b] != done {
			s.countdown(b)
		}
	}
}

func (s *Sudoku) countdown(b Block) {
	s.count[b]--
	switch s.count[b] {
	case 0:
		panic("No choice!!")
	case 1:
		for _, c := range children[b] {
			if s.state[c] == open {
				s.enqueue(c)
			}
		}
	}
}
