from algorithm import fill
from queues import Queue, initQueue, enqueue, dequeue, len
from strutils import repeat

const
  UNIT = 3
  SIZE = UNIT * UNIT
  DONE = SIZE + 1
  DIGITS = "123456789"

type
  View = enum GRD ROW COL BOX
  Coord = range[0 .. SIZE * SIZE * SIZE - 1]
  Group = range[0 .. 4 * SIZE * SIZE - 1]
  State = enum OPEN FIXED BANNED
  Count = range[0 .. DONE]
  Sudoku = ref object of RootObj
    state: array[Coord, State]
    count: array[Group, Count]
    queue: Queue[Coord]
  NoSolutionError = object of Exception

proc coord(i, j, k: int): Coord = (i * SIZE + j) * SIZE + k

proc group(v: View, p, q: int): Group = (v.ord * SIZE + p) * SIZE + q

var parents: array[Coord, seq[Group]]
for i in 0 ..< SIZE:
  for j in 0 ..< SIZE:
    let p = i div UNIT * UNIT + j div UNIT
    for k in 0 ..< SIZE:
      parents[coord(i, j, k)] = @[
        group(GRD, i, j),
        group(ROW, i, k),
        group(COL, j, k),
        group(BOX, p, k),
        ]

var children: array[Group, seq[Coord]]
for g, _ in children:
  children[g] = @[]
for c, gs in parents:
  for g in gs:
    children[g].add(c)

proc newSudoku(): Sudoku =
  result = new(Sudoku)
  result.state.fill(OPEN)
  result.count.fill(SIZE)
  result.queue = initQueue[Coord]()

proc newSudoku(s: Sudoku): Sudoku =
  result = new(Sudoku)
  result.state = s.state
  result.count = s.count
  result.queue = initQueue[Coord]()

proc fix(s: Sudoku, c: Coord): Sudoku {. discardable .} =
  result = s
  result.queue.enqueue(c)

proc read(s: Sudoku, input: string): Sudoku =
  result = s
  for i in 0 ..< SIZE:
    for j in 0 ..< SIZE:
      if i * SIZE + j < input.len:
        let k = DIGITS.find(input[i * SIZE + j])
        if k != -1:
          result.fix(coord(i, j, k))

proc show(s: Sudoku): string =
  result = repeat('.', SIZE * SIZE)
  for i in 0 ..< SIZE:
    for j in 0 ..< SIZE:
      for k in 0 ..< SIZE:
        if s.state[coord(i, j, k)] == FIXED:
          result[i * SIZE + j] = DIGITS[k]

proc search(s: Sudoku): Sudoku =

  while s.queue.len > 0:
    let c0 = s.queue.dequeue()
    if s.state[c0] == BANNED:
      raise new(NoSolutionError)
    if s.state[c0] == FIXED:
      continue
    s.state[c0] = FIXED
    for g1 in parents[c0]:
      s.count[g1] = DONE
      for c2 in children[g1]:
        if c2 != c0 and s.state[c2] == OPEN:
          s.state[c2] = BANNED
          for g3 in parents[c2]:
            if g3 != g1:
              s.count[g3] -= 1
              if s.count[g3] == 0:
                raise new(NoSolutionError)
              if s.count[g3] == 1:
                for c4 in children[g3]:
                  if s.state[c4] == OPEN:
                    s.fix(c4)

  var
    min = DONE
    gmin: Group
  for g, cnt in s.count:
    if cnt < min:
      min = cnt
      gmin = g

  if min == DONE:
    return s

  for c in children[gmin]:
    if s.state[c] == OPEN:
      try:
        return newSudoku(s).fix(c).search()
      except NoSolutionError:
        discard

  raise new(NoSolutionError)

proc solve(input: string): string =
  try:
    newSudoku().read(input).search().show()
  except NoSolutionError:
    "NO SOLUTION"

try:
  while true:
    echo stdin.readLine().solve()
except IOError:
  discard
