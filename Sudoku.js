'use strict';
(function() {

  const UNIT    = 3;
  const SIZE    = UNIT * UNIT;
  const GRD     = 0;
  const ROW     = 1;
  const COL     = 2;
  const BOX     = 3;
  const VIEW    = 4;
  const COORD   = SIZE * SIZE * SIZE;
  const BLOCK   = VIEW * SIZE * SIZE;
  const DEFINED = 0xDEF;
  const DIGITS  = '123456789';

  function coord(i, j, k) {
    return (i * SIZE + j) * SIZE + k;
  }

  function block(v, p, q) {
    return (v * SIZE + p) * SIZE + q;
  }

  const parents = new Array(COORD);
  for (let i = 0; i < SIZE; i++) {
    for (let j = 0; j < SIZE; j++) {
      const p = parseInt(i / UNIT) * UNIT + parseInt(j / UNIT);
      for (let k = 0; k < SIZE; k++) {
        parents[coord(i, j, k)] = [
          block(GRD, i, j),
          block(ROW, i, k),
          block(COL, j, k),
          block(BOX, p, k),
        ];
      }
    }
  }

  const children = new Array(BLOCK);
  for (let b = 0; b < BLOCK; b++)
    children[b] = [];
  for (let c = 0; c < COORD; c++)
    for (let b of parents[c])
      children[b].push(c);

  function Sudoku(that) {
    if (that) {
      this.admit = that.admit.slice();
      this.count = that.count.slice();
    }
    else {
      this.admit = new Array(COORD).fill(true);
      this.count = new Array(BLOCK).fill(SIZE);
    }
  }

  Sudoku.prototype.read = function(input) {
    for (let i = 0; i < SIZE; i++) {
      for (let j = 0; j < SIZE; j++) {
        const k = DIGITS.indexOf(input[i * SIZE + j]);
        if (k != -1)
          this.assign(coord(i, j, k))
      }
    }
    return this;
  };

  Sudoku.prototype.search = function() {

    let min = DEFINED;
    let bmin = -1;
    for (let b = 0; b < BLOCK; b++) {
      if (this.count[b] < min) {
        min = this.count[b];
        bmin = b;
      }
    }

    if (min == DEFINED)
      return this;

    for (let c of children[bmin]) {
      if (this.admit[c]) {
        try {
          return new Sudoku(this).assign(c).search();
        }
        catch (e) {
        }
      }
    }

    throw 'there is no solution';
  };

  Sudoku.prototype.show = function() {
    const output = [];
    for (let i = 0; i < SIZE; i++) {
      for (let j = 0; j < SIZE; j++) {
        const ks = [];
        for (let k = 0; k < SIZE; k++)
          if (this.admit[coord(i, j, k)])
            ks.push(k);
        if (ks.length == 1)
          output.push(DIGITS[ks[0]]);
        else
          output.push('.');
      }
    }
    return output.join('');
  };

  Sudoku.prototype.assign = function(c) {
    const queue = [c];
    while (queue.length > 0) {
      const c0 = queue.shift();
      if (!this.admit[c0])
        throw 'try to assign, but does not admit: ' + c0;
      for (let b1 of parents[c0]) {
        this.count[b1] = DEFINED;
        for (let c2 of children[b1]) {
          if (c2 != c0 && this.admit[c2]) {
            this.admit[c2] = false;
            for (let b3 of parents[c2]) {
              if (b3 != b1) {
                this.count[b3]--;
                if (this.count[b3] == 0)
                  throw 'there is no choice: ' + b3;
                if (this.count[b3] == 1)
                  for (let c4 of children[b3])
                    if (this.admit[c4])
                      queue.push(c4);
              }
            }
          }
        }
      }
    }
    return this;
  };

  function solve(input) {
    try {
      return new Sudoku().read(input).search().show();
    }
    catch (e) {
      return 'NO SOLUTION';
    }
  }

  const reader = require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
  });

  reader.on('line', input => console.log(solve(input)));
})();
