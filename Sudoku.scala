import scala.collection.mutable._

object Sudoku {

  def main(args: Array[String]): Unit = {
    for (line <- scala.io.Source.stdin.getLines())
      println(solve(line))
  }

  def solve(input: String): String = {
    try {
      new Sudoku().read(input).search().show()
    } catch {
      case _: NoSolutionException => "NO SOLUTION"
    }
  }

  private class NoSolutionException extends Exception

  private val GRD    = 0
  private val ROW    = 1
  private val COL    = 2
  private val BOX    = 3
  private val VIEW   = 4
  private val UNIT   = 3
  private val SIZE   = UNIT * UNIT
  private val COORD  = SIZE * SIZE * SIZE
  private val BLOCK  = VIEW * SIZE * SIZE
  private val OPEN   = 0
  private val FIXED  = 1
  private val BANNED = 2
  private val DONE   = SIZE + 1
  private val DIGITS = "123456789"

  private def coord(i: Int, j: Int, k: Int): Int = (i * SIZE + j) * SIZE + k

  private def block(v: Int, p: Int, q: Int): Int = (v * SIZE + p) * SIZE + q

  private val parents = Array.fill(COORD) { ArrayBuffer.empty[Int] }
  for {
    i <- 0 until SIZE
    j <- 0 until SIZE
    p = i / UNIT * UNIT + j / UNIT
    k <- 0 until SIZE
    c = coord(i, j, k)
  } {
    parents(c) += block(GRD, i, j)
    parents(c) += block(ROW, i, k)
    parents(c) += block(BOX, j, k)
    parents(c) += block(COL, p, k)
  }

  private val children = Array.fill(BLOCK) { ArrayBuffer.empty[Int] }
  for {
    c <- 0 until COORD
    b <- parents(c)
  } children(b) += c
}

class Sudoku private() {

  import Sudoku._

  private val state = Array.fill(COORD) { OPEN }
  private val count = Array.fill(BLOCK) { SIZE }
  private val queue = Queue[Int]()

  private def this(that: Sudoku) = {
    this()
    that.state.copyToArray(state)
    that.count.copyToArray(count)
  }

  private def fix(c: Int): Sudoku = {
    queue += c
    this
  }

  private def read(input: String): Sudoku = {
    for {
      i <- 0 until SIZE
      j <- 0 until SIZE
      if i * SIZE + j < input.length
      k = DIGITS.indexOf(input(i * SIZE + j))
      if k != -1
    } fix(coord(i, j, k))
  this
  }

  private def search(): Sudoku = {

    while (!queue.isEmpty) {
      val c0 = queue.dequeue
      if (state(c0) == BANNED)
        throw new NoSolutionException
      if (state(c0) == OPEN) {
        state(c0) = FIXED
        for (b1 <- parents(c0)) {
          count(b1) = DONE
          for (c2 <- children(b1) if c2 != c0 && state(c2) == OPEN) {
            state(c2) = BANNED
            for (b3 <- parents(c2) if b3 != b1) {
              count(b3) -= 1
              if (count(b3) == 0)
                throw new NoSolutionException
              if (count(b3) == 1)
                for (c4 <- children(b3) if state(c4) == OPEN)
                  fix(c4)
            }
          }
        }
      }
    }

    val min = count.min
    if (min == DONE)
      return this

    val b = count.indexOf(min)
    for (c <- children(b) if state(c) == OPEN) {
      try {
        return new Sudoku(this).fix(c).search()
      } catch {
        case _: NoSolutionException => ()
      }
    }

    throw new NoSolutionException
  }

  private def show(): String = {
    val output = Array.fill(SIZE * SIZE) { '.' }
    for {
      i <- 0 until SIZE
      j <- 0 until SIZE
      k <- 0 until SIZE
      if state(coord(i, j, k)) == FIXED
    } output(i * SIZE + j) = DIGITS(k)
    output.mkString
  }
}
