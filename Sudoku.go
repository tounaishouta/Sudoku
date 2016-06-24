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
	if s := NewSudoku(); s.Read(input) {
		if s, ok := s.Search(); ok {
			return s.String()
		}
	}
	return "NO SOLUTION"
}

const (
	grd     = iota
	row     = iota
	col     = iota
	box     = iota
	view    = iota
	unit    = 3
	size    = unit * unit
	nCoord  = size * size * size
	nBlock  = view * size * size
	defined = 0xDEF
	digits  = "123456789"
)

type (
	coord  int
	block  int
	sudoku struct {
		admit [nCoord]bool
		count [nBlock]int
	}
)

var (
	parents  [nCoord][]block
	children [nBlock][]coord
)

func coordOf(i, j, k int) coord {
	return coord((i*size+j)*size + k)
}

func blockOf(v, p, q int) block {
	return block((v*size+p)*size + q)
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
	for c, bs := range parents {
		for _, b := range bs {
			children[b] = append(children[b], coord(c))
		}
	}
}

func NewSudoku() *sudoku {
	s := new(sudoku)
	for c, _ := range s.admit {
		s.admit[c] = true
	}
	for b, _ := range s.count {
		s.count[b] = size
	}
	return s
}

func (s *sudoku) Read(input string) bool {
	for ij, r := range input {
		if ij >= size*size {
			break
		}
		if k := strings.IndexRune(digits, r); k != -1 {
			if !s.Assign(coordOf(ij/size, ij%size, k)) {
				return false
			}
		}
	}
	return true
}

func (s *sudoku) Search() (*sudoku, bool) {

	min, bmin := defined, -1
	for b, cnt := range s.count {
		if cnt < min {
			min = cnt
			bmin = b
		}
	}
	if min == defined {
		return s, true
	}

	for _, c := range children[bmin] {
		if s.admit[c] {
			if ss := s.Clone(); ss.Assign(c) {
				if ss, ok := ss.Search(); ok {
					return ss, true
				}
			}
		}
	}

	return nil, false
}

func (s *sudoku) String() string {
	output := make([]rune, size*size)
	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			ks := make([]int, 0)
			for k := 0; k < size; k++ {
				if s.admit[coordOf(i, j, k)] {
					ks = append(ks, k)
				}
			}
			if len(ks) == 1 {
				output[i*size+j] = rune(digits[ks[0]])
			} else {
				output[i*size+j] = '.'
			}
		}
	}
	return string(output)
}

func (s *sudoku) Clone() *sudoku {
	ss := new(sudoku)
	for c, ok := range s.admit {
		ss.admit[c] = ok
	}
	for b, cnt := range s.count {
		ss.count[b] = cnt
	}
	return ss
}

func (s *sudoku) Assign(c coord) bool {
	queue := []coord{c}
	for len(queue) > 0 {
		c0 := queue[0]
		queue = queue[1:]
		if !s.admit[c0] {
			return false
		}
		for _, b1 := range parents[c0] {
			s.count[b1] = defined
			for _, c2 := range children[b1] {
				if c2 != c0 && s.admit[c2] {
					s.admit[c2] = false
					for _, b3 := range parents[c2] {
						if b3 != b1 {
							s.count[b3]--
							if s.count[b3] == 0 {
								return false
							}
							if s.count[b3] == 1 {
								for _, c4 := range children[b3] {
									if s.admit[c4] {
										queue = append(queue, c4)
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return true
}
