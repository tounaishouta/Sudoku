'use strict';

const GRD   = 0;
const ROW   = 1;
const COL   = 2;
const BOX   = 3;
const VIEW  = 4;
const UNIT  = 3;
const SIZE  = UNIT * UNIT;
const COORD = SIZE * SIZE * SIZE;
const BLOCK = VIEW * SIZE * SIZE;

const OPEN   = 0;
const FIXED  = 1;
const BANNED = 2;

const DONE = SIZE + 1;

const DIGITS = '123456789';

const NO_SOLUTION = "NO SOLUTION";

const coord = (i, j, k) => (i * SIZE + j) * SIZE + k;
const block = (v, p, q) => (v * SIZE + p) * SIZE + q;

const parents = Array(COORD);
for (let i = 0; i < SIZE; i++) {
  for (let j = 0; j < SIZE; j++) {
    const p = Math.floor(i / UNIT) * UNIT + Math.floor(j / UNIT);
    for (let k = 0; k < SIZE; k++)
      parents[coord(i, j, k)] = [
        block(GRD, i, j),
        block(ROW, i, k),
        block(COL, j, k),
        block(BOX, p, k),
      ];
  }
}

const children = Array(BLOCK);
for (let b = 0; b < BLOCK; b++)
  children[b] = Array();
for (let c = 0; c < COORD; c++)
  for (let b of parents[c])
    children[b].push(c);

class Sudoku {

  constructor(that) {
    if (that) {
      this.state = that.state.slice();
      this.count = that.count.slice();
    }
    else {
      this.state = Array(COORD).fill(OPEN);
      this.count = Array(BLOCK).fill(SIZE);
    }
    this.queue = [];
  }

  fix(c) {
    this.queue.push(c);
    return this;
  }

  read(input) {
    for (let i = 0; i < SIZE; i++) {
      for (let j = 0; j < SIZE; j++) {
        if (i * SIZE + j < input.length) {
          const k = DIGITS.indexOf(input[i * SIZE + j]);
          if (k !== -1)
            this.fix(coord(i, j, k));
        }
      }
    }
    return this;
  }

  search() {

    while (this.queue.length > 0) {
      const c0 = this.queue.shift();
      if (this.state[c0] === FIXED)
        continue;
      if (this.state[c0] === BANNED)
        throw NO_SOLUTION;
      this.state[c0] = FIXED;
      for (let b1 of parents[c0]) {
        this.count[b1] = DONE;
        for (let c2 of children[b1]) {
          if (c2 !== c0 && this.state[c2] === OPEN) {
            this.state[c2] = BANNED;
            for (let b3 of parents[c2]) {
              if (b3 !== b1) {
                this.count[b3]--;
                if (this.count[b3] === 0)
                  throw NO_SOLUTION;
                if (this.count[b3] === 1)
                  for (let c4 of children[b3])
                    if (this.state[c4] === OPEN)
                      this.fix(c4);
              }
            }
          }
        }
      }
    }

    const min = Math.min.apply(null, this.count);
    if (min === DONE)
      return this;

    const b = this.count.indexOf(min);
    for (let c of children[b]) {
      if (this.state[c] === OPEN) {
        try {
          return new Sudoku(this).fix(c).search();
        }
        catch (e) {
          console.assert(e === NO_SOLUTION);
        }
      }
    }

    throw NO_SOLUTION;
  }

  show() {
    const output = Array(SIZE * SIZE).fill('.');
    for (let i = 0; i < SIZE; i++)
      for (let j = 0; j < SIZE; j++)
        for (let k = 0; k < SIZE; k++)
          if (this.state[coord(i, j, k)] === FIXED)
            output[i * SIZE + j] = DIGITS[k];
    return output.join('');
  }
}

function solve(input) {
  try {
    return new Sudoku().read(input).search().show();
  }
  catch (e) {
    console.assert(e === NO_SOLUTION);
    return e;
  }
}

require('readline')
  .createInterface({ input: process.stdin, terminal: false })
  .on('line', input => console.log(solve(input)));
