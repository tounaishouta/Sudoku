package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		if s, ok := Solve(scanner.Text()); ok {
			fmt.Println(s)
		} else {
			fmt.Println("no solution")
		}
	}
}

func Solve(input string) (*sudoku, bool) {
	s := New()
	if s.Read(input) && s.Solve() {
		return s, true
	} else {
		return nil, false
	}
}

func New() *sudoku {
	s := new(sudoku)
	for c, _ := range s.admits {
		s.admits[c] = true
	}
	for b, _ := range s.option {
		s.option[b] = nDigit
	}
	return s
}

func (s *sudoku) Read(input string) bool {
	for ij, r := range input {
		if ij >= nDigit*nDigit {
			break
		}
		if r < '1' || r > '9' {
			continue
		}
		i := ij / nDigit
		j := ij % nDigit
		k := int(r) - int('1')
		if !s.assign(coordOf(i, j, k)) {
			return false
		}
	}
	return true
}

func (s *sudoku) Solve() bool {
	min := defined
	for _, o := range s.option {
		if o < min {
			min = o
		}
	}
	if min == defined {
		return true
	}
	b := 0
	for s.option[b] != min {
		b++
	}
	ss := New()
	for _, c := range children[b] {
		if s.admits[c] {
			ss.copyFrom(s)
			if ss.assign(c) && ss.Solve() {
				s.copyFrom(ss)
				return true
			}
		}
	}
	return false
}

func (s *sudoku) String() string {
	sl := make([]rune, 0, nDigit*nDigit)
	for i := 0; i < nDigit; i++ {
		for j := 0; j < nDigit; j++ {
			if s.option[blockOf(grd, i, j)] == defined {
				k := 0
				for !s.admits[coordOf(i, j, k)] {
					k++
				}
				sl = append(sl, rune(int('1')+k))
			} else {
				sl = append(sl, '.')
			}
		}
	}
	return string(sl)
}

type (
	sudoku struct {
		admits [nCoord]bool
		option [nBlock]int
	}
	coord int
	block int
)

const (
	grd     = iota
	row     = iota
	col     = iota
	box     = iota
	nView   = iota
	size    = 3
	nDigit  = size * size
	nCoord  = nDigit * nDigit * nDigit
	nBlock  = nView * nDigit * nDigit
	defined = 0xDEF
)

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
	for c, bs := range parents {
		for _, b := range bs {
			for _, cc := range children[b] {
				if cc != coord(c) {
					siblings[c] = append(siblings[c], cc)
				}
			}
		}
	}
}

func coordOf(i, j, k int) coord {
	return coord((i*nDigit+j)*nDigit + k)
}

func blockOf(v, i, j int) block {
	return block((v*nDigit+i)*nDigit + j)
}

func (s *sudoku) copyFrom(ss *sudoku) {
	for c, a := range ss.admits {
		s.admits[c] = a
	}
	for b, o := range ss.option {
		s.option[b] = o
	}
}

func (s *sudoku) assign(c coord) bool {
	if !s.admits[c] {
		return false
	}
	bs := make([]block, 0)
	for _, cc := range siblings[c] {
		if s.admits[cc] {
			s.admits[cc] = false
			for _, b := range parents[cc] {
				s.option[b]--
				bs = append(bs, b)
			}
		}
	}
	for _, b := range parents[c] {
		s.option[b] = defined
	}
	for _, b := range bs {
		if !s.check(b) {
			return false
		}
	}
	return true
}

func (s *sudoku) check(b block) bool {
	switch s.option[b] {
	case 0:
		return false
	case 1:
		i := 0
		for !s.admits[children[b][i]] {
			i++
		}
		return s.assign(children[b][i])
	default:
		return true
	}
}
