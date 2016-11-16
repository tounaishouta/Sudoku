from algorithm import fill
from queues import Queue, initQueue, enqueue, dequeue, len
from strutils import repeat

const
  unit = 3
  size = unit * unit
  done = size + 1
  digits = "123456789"

type
  View = enum grd, row, col, box
  Coord = range[0 .. size * size * size - 1]
  Group = range[0 .. 4 * size * size - 1]
  State = enum open, fixed, banned
  Count = range[0 .. done]
  Sudoku = ref object of RootObj
    state: array[Coord, State]
    count: array[Group, Count]
    queue: Queue[Coord]
  NoSolutionError = ref object of Exception

proc coord(i: int, j: int, k:int): Coord =
  (i * size + j) * size + k

proc group(v: View, p: int, q: int): Group =
  (v.ord * size + p) * size + q

var
  parents: array[Coord, seq[Group]]
  children: array[Group, seq[Coord]]

for i in 0 ..< size:
  for j in 0 ..< size:
    let p = i div unit * unit + j div unit
    for k in 0 ..< size:
      parents[coord(i, j, k)] = @[
        group(grd, i, j),
        group(row, i, k),
        group(col, j, k),
        group(box, p, k),
        ]

for g, _ in children:
  children[g] = @[]
for c, gs in parents:
  for g in gs:
    children[g].add(c)

proc newSudoku(): Sudoku =
  new result
  result.state.fill(open)
  result.count.fill(size)
  result.queue = initQueue[Coord]()

proc newSudoku(s: Sudoku): Sudoku =
  new result
  result.state = s.state
  result.count = s.count
  result.queue = initQueue[Coord]()

proc enqueue(s: Sudoku, c: Coord): Sudoku {.discardable.} =
  result = s
  result.queue.enqueue(c)

proc read(s: Sudoku, input: string): Sudoku =
  result = s
  for i in 0 ..< size:
    for j in 0 ..< size:
      if i * size + j < input.len:
        let k = digits.find(input[i * size + j])
        if k != -1:
          result.enqueue(coord(i, j, k))

proc `$`(s: Sudoku): string =
  result = repeat('.', size * size)
  for i in 0 ..< size:
    for j in 0 ..< size:
      for k in 0 ..< size:
        if s.state[coord(i, j, k)] == fixed:
          result[i * size + j] = digits[k]

proc fix(s: Sudoku, c: Coord)
proc mark(s: Sudoku, g: Group)
proc ban(s: Sudoku, c: Coord)
proc dec(s: Sudoku, g: Group)

proc search(s: Sudoku): Sudoku =

  while s.queue.len > 0:
    let c = s.queue.dequeue
    case s.state[c]
    of open: s.fix(c)
    of fixed: continue
    of banned: raise new NoSolutionError

  var
    min = done
    gmin: Group
  for g, cnt in s.count:
    if cnt < min:
      min = cnt
      gmin = g

  if min == done:
    return s

  for c in children[gmin]:
    if s.state[c] == open:
      try:
        return newSudoku(s).enqueue(c).search()
      except NoSolutionError:
        discard

  raise new NoSolutionError

proc fix(s: Sudoku, c: Coord) =
  s.state[c] = fixed
  for g in parents[c]:
    s.mark(g)

proc mark(s: Sudoku, g: Group) =
  s.count[g] = done
  for c in children[g]:
    if s.state[c] == open:
      s.ban(c)

proc ban(s: Sudoku, c: Coord) =
  s.state[c] = banned
  for g in parents[c]:
    if s.count[g] != done:
      s.dec(g)

proc dec(s: Sudoku, g: Group) =
  s.count[g].dec
  case s.count[g]
  of 0:
    raise new NoSolutionError
  of 1:
    for c in children[g]:
      if s.state[c] == open:
        s.enqueue(c)
  else:
    discard

proc solve(input: string): string =
  try:
    result = $newSudoku().read(input).search()
  except NoSolutionError:
    result = "NO SOLUTION"

try:
  while true:
    echo stdin.readLine().solve()
except IOError:
  discard
