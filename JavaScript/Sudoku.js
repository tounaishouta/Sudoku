'use strict';

const VIEW  = { GRD: 0, ROW: 1, COL: 2, BOX: 3 };
const STATE = { OPEN: 0, FIXED: 1, BANNED: 2 };

const UNIT  = 3;
const SIZE  = UNIT * UNIT;
const COORD = SIZE * SIZE * SIZE;
const BLOCK = Object.keys(VIEW).length * SIZE * SIZE;
const DONE  = SIZE + 1;

const DIGITS = '123456789';

const NO_SOLUTION = 'NO SOLUTION';

const coord = (i, j, k) => (i * SIZE + j) * SIZE + k;
const block = (v, p, q) => (v * SIZE + p) * SIZE + q;

const parents = Array(COORD);
for (let i = 0; i < SIZE; i++) {
  for (let j = 0; j < SIZE; j++) {
    const p = Math.floor(i / UNIT) * UNIT + Math.floor(j / UNIT);
    for (let k = 0; k < SIZE; k++)
      parents[coord(i, j, k)] = [
        block(VIEW.GRD, i, j),
        block(VIEW.ROW, i, k),
        block(VIEW.COL, j, k),
        block(VIEW.BOX, p, k),
      ];
  }
}

const children = Array(BLOCK);
for (let b = 0; b < BLOCK; b++)
  children[b] = Array();
for (let c = 0; c < COORD; c++)
  for (const b of parents[c])
    children[b].push(c);

class Sudoku {

  constructor(that) {
    if (that) {
      this.state = that.state.slice();
      this.count = that.count.slice();
    }
    else {
      this.state = Array(COORD).fill(STATE.OPEN);
      this.count = Array(BLOCK).fill(SIZE);
    }
    this.queue = [];
  }

  enqueue(c) {
    this.queue.push(c);
    return this;
  }

  read(input) {
    for (let i = 0; i < SIZE; i++) {
      for (let j = 0; j < SIZE; j++) {
        if (i * SIZE + j < input.length) {
          const k = DIGITS.indexOf(input[i * SIZE + j]);
          if (k !== -1)
            this.enqueue(coord(i, j, k));
        }
      }
    }
    return this;
  }

  show() {
    const output = Array(SIZE * SIZE).fill('.');
    for (let i = 0; i < SIZE; i++)
      for (let j = 0; j < SIZE; j++)
        for (let k = 0; k < SIZE; k++)
          if (this.state[coord(i, j, k)] === STATE.FIXED)
            output[i * SIZE + j] = DIGITS[k];
    return output.join('');
  }

  search() {

    while (this.queue.length > 0) {
      const c = this.queue.shift();
      switch (this.state[c]) {
        case STATE.OPEN:
          this.fix(c);
          break;
        case STATE.FIXED:
          break;
        case STATE.BANNED:
          throw NO_SOLUTION;
      }
    }

    const min = Math.min.apply(null, this.count);
    if (min === DONE)
      return this;

    const b = this.count.indexOf(min);
    for (const c of children[b]) {
      if (this.state[c] === STATE.OPEN) {
        try {
          return new Sudoku(this).enqueue(c).search();
        }
        catch (e) {
          if (e !== NO_SOLUTION)
            throw e;
        }
      }
    }

    throw NO_SOLUTION;
  }

  fix(c) {
      this.state[c] = STATE.FIXED;
      for (const b of parents[c])
        this.mark(b);
  }

  mark(b) {
    this.count[b] = DONE;
    for (const c of children[b])
      if (this.state[c] == STATE.OPEN)
        this.ban(c);
  }

  ban(c) {
    this.state[c] = STATE.BANNED;
    for (const b of parents[c])
      if (this.count[b] != DONE)
        this.countdown(b);
  }

  countdown(b) {
    this.count[b]--;
    if (this.count[b] == 0)
      throw NO_SOLUTION;
    if (this.count[b] == 1)
      for (const c of children[b])
        if (this.state[c] == STATE.OPEN)
          this.enqueue(c);
  }
}

function solve(input) {
  try {
    return new Sudoku().read(input).search().show();
  }
  catch (e) {
    if (e !== NO_SOLUTION)
      throw e;
    return e;
  }
}

require('readline')
  .createInterface({ input: process.stdin, terminal: false })
  .on('line', input => console.log(solve(input)));
