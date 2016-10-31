package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		fmt.Println(Solve(scanner.Text()))
	}
}

func Solve(input string) string {
	if s, ok := NewSudoku().Read(input).Search(); ok {
		return s.String()
	} else {
		return "NO SOLUTION"
	}
}

const (
	grd  = 0
	row  = 1
	col  = 2
	box  = 3
	view = 4

	unit   = 3
	size   = unit * unit
	nBlock = view * size * size
	nCoord = size * size * size

	done = size + 1

	open   = 0
	fixed  = 1
	banned = 2

	digits = "123456789"
)

type (
	block  int
	coord  int
	sudoku struct {
		state [nCoord]int
		count [nBlock]int
		queue []coord
	}
)

var (
	parents  [nCoord][]block
	children [nBlock][]coord
)

func blockOf(v, p, q int) block {
	return block((v*size+p)*size + q)
}

func coordOf(i, j, k int) coord {
	return coord((i*size+j)*size + k)
}

func init() {
	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			p := i/unit*unit + j/unit
			for k := 0; k < size; k++ {
				parents[coordOf(i, j, k)] = []block{
					blockOf(grd, i, j),
					blockOf(row, i, k),
					blockOf(col, j, k),
					blockOf(box, p, k),
				}
			}
		}
	}
	for c := range parents {
		for _, b := range parents[c] {
			children[b] = append(children[b], coord(c))
		}
	}
}

func NewSudoku() *sudoku {
	s := new(sudoku)
	for b := range s.count {
		s.count[b] = size
	}
	for c := range s.state {
		s.state[c] = open
	}
	return s
}

func (s *sudoku) Clone() *sudoku {
	ss := new(sudoku)
	for b, v := range s.count {
		ss.count[b] = v
	}
	for c, v := range s.state {
		ss.state[c] = v
	}
	return ss
}

func (s *sudoku) Fix(c coord) *sudoku {
	s.queue = append(s.queue, c)
	return s
}

func (s *sudoku) Read(input string) *sudoku {
	for ij, r := range input {
		if ij < size*size {
			i := ij / size
			j := ij % size
			k := strings.IndexRune(digits, r)
			if k != -1 {
				s.Fix(coordOf(i, j, k))
			}
		}
	}
	return s
}

func (s *sudoku) Search() (*sudoku, bool) {

	for len(s.queue) > 0 {
		c0 := s.queue[0]
		s.queue = s.queue[1:]
		if s.state[c0] == fixed {
			continue
		}
		if s.state[c0] == banned {
			return nil, false
		}
		s.state[c0] = fixed
		for _, b1 := range parents[c0] {
			s.count[b1] = done
			for _, c2 := range children[b1] {
				if c2 != c0 && s.state[c2] != banned {
					s.state[c2] = banned
					for _, b3 := range parents[c2] {
						if b3 != b1 {
							s.count[b3]--
							if s.count[b3] == 0 {
								return nil, false
							}
							if s.count[b3] == 1 {
								for _, c4 := range children[b3] {
									if s.state[c4] == open {
										s.Fix(c4)
									}
								}
							}
						}
					}
				}
			}
		}
	}

	min, bmin := done, -1
	for b, v := range s.count {
		if v < min {
			min, bmin = v, b
		}
	}

	if min == done {
		return s, true
	}

	for _, c := range children[bmin] {
		if s.state[c] == open {
			if ss, ok := s.Clone().Fix(c).Search(); ok {
				return ss, true
			}
		}
	}

	return nil, false
}

func (s *sudoku) String() string {
	output := make([]rune, size*size)
	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			output[i*size+j] = '.'
			for k := 0; k < size; k++ {
				if s.state[coordOf(i, j, k)] == fixed {
					output[i*size+j] = rune(digits[k])
				}
			}
		}
	}
	return string(output)
}
