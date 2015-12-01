package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	for scanner := bufio.NewScanner(os.Stdin); scanner.Scan(); {
		if s, ok := NewSudoku(scanner.Text()); ok {
			if s, ok := s.Solve(); ok {
				fmt.Println(s)
				continue
			}
		}
		fmt.Println("failed to solve")
	}
}

type sudoku struct {
	admits [nCoord]bool
	choice [nBlock]int
}

type coord int
type block int

const (
	size    = 3
	nDigit  = size * size
	nCoord  = nDigit * nDigit * nDigit
	nBlock  = nView * nDigit * nDigit
	defined = 999
)

const (
	grd = iota
	row
	col
	box
	nView
)

func toCoord(i, j, k int) coord {
	return coord((i*nDigit+j)*nDigit + k)
}

func toBlock(v, p, q int) block {
	return block((v*nDigit+p)*nDigit + q)
}

func NewSudoku(input string) (*sudoku, bool) {
	s := new(sudoku)
	for c := 0; c < nCoord; c++ {
		s.admits[c] = true
	}
	for b := 0; b < nBlock; b++ {
		s.choice[b] = nDigit
	}
	for p, r := range input {
		if p >= nDigit*nDigit {
			break
		}
		i, j, k := p/nDigit, p%nDigit, int(r-'1')
		if 0 <= k && k < nDigit {
			if s1, ok := s.assign(toCoord(i, j, k)); ok {
				s = s1
			} else {
				return nil, false
			}
		}
	}
	return s, true
}

func (s *sudoku) Solve() (*sudoku, bool) {
	var b0 block
	min := defined
	for b := 0; b < nBlock; b++ {
		if s.choice[b] < min {
			min = s.choice[b]
			b0 = block(b)
		}
	}
	if min == defined {
		return s, true
	}
	for _, c := range children[b0] {
		if s.admits[c] {
			if s1, ok := s.clone().assign(c); ok {
				if s1, ok := s1.Solve(); ok {
					return s1, true
				}
			}
		}
	}
	return nil, false
}

func (s *sudoku) String() string {
	output := make([]rune, 0, nDigit*nDigit)
	for i := 0; i < nDigit; i++ {
		for j := 0; j < nDigit; j++ {
			if s.choice[toBlock(grd, i, j)] == defined {
				var k0 int
				for k := 0; k < nDigit; k++ {
					if s.admits[toCoord(i, j, k)] {
						k0 = k
					}
				}
				output = append(output, rune('1'+k0))
			} else {
				output = append(output, '.')
			}
		}
	}
	return string(output)
}

func (s *sudoku) assign(c0 coord) (*sudoku, bool) {
	if !s.admits[c0] {
		return nil, false
	}
	bs := make([]block, 0)
	for _, c := range siblings[c0] {
		if s.admits[c] {
			s.admits[c] = false
			for _, b := range parents[c] {
				s.choice[b]--
				bs = append(bs, b)
			}
		}
	}
	for _, b := range parents[c0] {
		s.choice[b] = defined
	}
	for _, b := range bs {
		if s1, ok := s.check(b); ok {
			s = s1
		} else {
			return nil, false
		}
	}
	return s, true
}

func (s *sudoku) check(b block) (*sudoku, bool) {
	switch s.choice[b] {
	case 0:
		return nil, false
	case 1:
		var c0 coord
		for _, c := range children[b] {
			if s.admits[c] {
				c0 = c
			}
		}
		return s.assign(c0)
	default:
		return s, true
	}
}

func (s0 *sudoku) clone() *sudoku {
	s := new(sudoku)
	for c := 0; c < nCoord; c++ {
		s.admits[c] = s0.admits[c]
	}
	for b := 0; b < nBlock; b++ {
		s.choice[b] = s0.choice[b]
	}
	return s
}

var (
	parents  [nCoord][]block
	children [nBlock][]coord
	siblings [nCoord][]coord
)

func init() {
	for i := 0; i < nDigit; i++ {
		for j := 0; j < nDigit; j++ {
			p := i/size*size + j/size
			for k := 0; k < nDigit; k++ {
				parents[toCoord(i, j, k)] = []block{
					toBlock(grd, i, j),
					toBlock(row, i, k),
					toBlock(col, j, k),
					toBlock(box, p, k),
				}
			}
		}
	}
	for c := 0; c < nCoord; c++ {
		for _, b := range parents[c] {
			children[b] = append(children[b], coord(c))
		}
	}
	for c0 := 0; c0 < nCoord; c0++ {
		for _, b := range parents[c0] {
			for _, c := range children[b] {
				if c != coord(c0) {
					siblings[c0] = append(siblings[c0], c)
				}
			}
		}
	}
}
