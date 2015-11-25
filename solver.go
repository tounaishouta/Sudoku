package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	for scanner := bufio.NewScanner(os.Stdin); scanner.Scan(); {
		input := scanner.Text()
		if s, ok := NewSudoku(input); ok {
			if s, ok := s.Solve(); ok {
				fmt.Println(s)
				continue
			}
			fmt.Println("fail to Solve")
			continue
		}
		fmt.Println("fail to NewSudoku")
		continue
	}
}

type coord int
type block int

const (
	grd = iota
	row
	col
	box
	nView
)

const (
	size   = 3
	nDigit = size * size
	nCoord = nDigit * nDigit * nDigit
	nBlock = nView * nDigit * nDigit
)

const defined = 99

type sudoku struct {
	admits [nCoord]bool
	choice [nBlock]int
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
				c := toCoord(i, j, k)
				parents[c] = []block{toBlock(grd, i, j), toBlock(row, i, k), toBlock(col, j, k), toBlock(box, p, k)}
			}
		}
	}
	for c := 0; c < nCoord; c++ {
		for _, b := range parents[c] {
			children[b] = append(children[b], coord(c))
		}
	}
	for c := 0; c < nCoord; c++ {
		cs := make(map[coord]bool)
		for _, b := range parents[c] {
			for _, cc := range children[b] {
				cs[cc] = true
			}
		}
		for cc, _ := range cs {
			if cc != coord(c) {
				siblings[c] = append(siblings[c], cc)
			}
		}
	}
}

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
			if ss, ok := s.assign(toCoord(i, j, k)); ok {
				s = ss
			} else {
				return nil, false
			}
		}
	}
	return s, true
}

func (s *sudoku) String() string {
	output := make([]rune, 0, nDigit*nDigit)
	for i := 0; i < nDigit; i++ {
		for j := 0; j < nDigit; j++ {
			var k int
			n := 0
			for kk := 0; kk < nDigit; kk++ {
				if s.admits[toCoord(i, j, kk)] {
					k = kk
					n++
				}
			}
			switch n {
			case 0:
				output = append(output, 'x')
			case 1:
				output = append(output, rune('1'+k))
			default:
				output = append(output, '.')
			}
		}
	}
	return string(output)
}

func (s *sudoku) Solve() (*sudoku, bool) {
	var bmin block
	min := defined
	for b := 0; b < nBlock; b++ {
		if s.choice[b] < min {
			min, bmin = s.choice[b], block(b)
		}
	}
	if min == defined {
		return s, true
	}
	for _, c := range children[bmin] {
		if s.admits[c] {
			if ss, ok := s.clone().assign(c); ok {
				if sss, ok := ss.Solve(); ok {
					return sss, true
				}
			}
		}
	}
	return nil, false
}

func (s *sudoku) clone() *sudoku {
	ss := new(sudoku)
	for c := 0; c < nCoord; c++ {
		ss.admits[c] = s.admits[c]
	}
	for b := 0; b < nBlock; b++ {
		ss.choice[b] = s.choice[b]
	}
	return ss
}

func (s *sudoku) assign(c coord) (*sudoku, bool) {
	if !s.admits[c] {
		return nil, false
	}
	bs := make([]block, 0)
	for _, cc := range siblings[c] {
		if s.admits[cc] {
			s.admits[cc] = false
			for _, b := range parents[cc] {
				s.choice[b]--
				bs = append(bs, b)
			}
		}
	}
	for _, b := range parents[c] {
		s.choice[b] = defined
	}
	for _, b := range bs {
		if ss, ok := s.check(b); ok {
			s = ss
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
		var c coord
		for _, c = range children[b] {
			if s.admits[c] {
				break
			}
		}
		return s.assign(c)
	default:
		return s, true
	}
}
