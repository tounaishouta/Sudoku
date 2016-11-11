from algorithm import fill
from strutils import repeat
from queues import Queue, initQueue, len, enqueue, dequeue

const
  unit   = 3
  size   = unit * unit
  nview  = 4
  ncoord = size * size * size
  nblock = nview * size * size
  done   = size + 1
  digits = "123456789"

type
  View   = enum grd, row, col, box
  Coord  = range[0..ncoord-1]
  Block  = range[0..nblock-1]
  State  = enum open, fixed, banned
  Count  = range[0..done]
  Sudoku = ref object of RootObj
    state: array[Coord, State]
    count: array[Block, Count]
    queue: Queue[Coord]
  NoSolutionException = object of Exception

var
  parents:  array[Coord, seq[Block]]
  children: array[Block, seq[Coord]]

proc coordOf(i, j, k: int): Coord = (i * size + j) * size + k

proc blockOf(v: View, p, q: int): Block = (v.ord * size + p) * size + q

for i in 0..<size:
  for j in 0..<size:
    let p = i div unit * unit + j div unit
    for k in 0..<size:
      parents[coordOf(i, j, k)] = @[
        blockOf(grd, i, j),
        blockOf(row, i, k),
        blockOf(col, j, k),
        blockOf(box, p, k)]

for b, _ in children:
  children[b] = @[]
for c, bs in parents:
  for b in bs:
    children[b].add(c)

proc newSudoku(): Sudoku =
  let s = new(Sudoku)
  s.state.fill(open)
  s.count.fill(size)
  s.queue = initQueue[Coord]()
  return s

proc clone(s: Sudoku): Sudoku =
  let ss = new(Sudoku)
  ss.state = s.state
  ss.count = s.count
  ss.queue = initQueue[Coord]()
  return ss

proc fix(s: Sudoku, c: Coord): Sudoku =
  s.queue.enqueue(c)
  return s

proc read(s: Sudoku, input: string): Sudoku =
  for i in 0..<size:
    for j in 0..<size:
      if i * size + j < input.len:
        let k = digits.find(input[i * size + j])
        if k != -1:
          discard s.fix(coordOf(i, j, k))
  return s

proc show(s: Sudoku): string =
  result = repeat('.', size * size)
  for i in 0..<size:
    for j in 0..<size:
      for k in 0..<size:
        if s.state[coordOf(i, j, k)] == fixed:
          result[i * size + j] = digits[k]

proc search(s: Sudoku): Sudoku =

  while s.queue.len > 0:
    let c0 = s.queue.dequeue()
    if s.state[c0] == fixed:
      continue
    if s.state[c0] == banned:
      raise new(NoSolutionException)
    s.state[c0] = fixed
    for b1 in parents[c0]:
      s.count[b1] = done
      for c2 in children[b1]:
        if c2 != c0 and s.state[c2] == open:
          s.state[c2] = banned
          for b3 in parents[c2]:
            if b3 != b1:
              s.count[b3] -= 1
              if s.count[b3] == 0:
                raise new(NoSolutionException)
              if s.count[b3] == 1:
                for c4 in children[b3]:
                  if s.state[c4] == open:
                    discard s.fix(c4)

  var
    min = done
    bmin: Block
  for b, cnt in s.count:
    if cnt < min:
      min = cnt
      bmin = b

  if min == done:
    return s

  for c in children[bmin]:
    if s.state[c] == open:
      try:
        return s.clone().fix(c).search()
      except NoSolutionException:
        discard

  raise new(NoSolutionException)

proc solve(input: string): string =
  try:
    return newSudoku().read(input).search().show()
  except NoSolutionException:
    return "NO SOLUTION"

try:
  while true:
    echo stdin.readLine().solve()
except IOError:
  discard
