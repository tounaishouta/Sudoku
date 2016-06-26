import scala.collection.mutable._

object Sudoku {

  def main(args: Array[String]): Unit = {
    for (line <- scala.io.Source.stdin.getLines()) {
      println(solve(line))
    }
  }

  def solve(input: String): String = {
    try {
      new Sudoku().read(input).search().show()
    } catch {
      case _: NoSolutionException => "NO SOLUTION"
    }
  }

  private class NoSolutionException extends Exception

  private val UNIT    = 3
  private val SIZE    = UNIT * UNIT
  private val GRD     = 0
  private val ROW     = 1
  private val COL     = 2
  private val BOX     = 3
  private val VIEW    = 4
  private val COORD   = SIZE * SIZE * SIZE
  private val BLOCK   = VIEW * SIZE * SIZE
  private val DEFINED = 0xDEF
  private val DIGITS  = "123456789"

  private def block(i: Int, j: Int, k: Int): Int = (i * SIZE + j) * SIZE + k
  private def coord(v: Int, p: Int, q: Int): Int = (v * SIZE + p) * SIZE + q

  private val parents = Array.tabulate(COORD) { c =>
    val i = c / SIZE / SIZE
    val j = c / SIZE % SIZE
    val k = c % SIZE
    val p = i / UNIT * UNIT + j / UNIT
    Array(block(GRD, i, j), block(ROW, i, k), block(COL, j, k), block(BOX, p, k))
  }

  private val children = Array.fill(BLOCK) { ArrayBuffer.empty[Int] }

  for (c <- 0 until COORD ; b <- parents(c)) {
    children(b) += c
  }
}

class Sudoku private() {

  import Sudoku._

  private val admit = Array.fill(COORD) { true }
  private val count = Array.fill(BLOCK) { SIZE }

  private def this(that: Sudoku) = {
    this()
    that.admit.copyToArray(admit)
    that.count.copyToArray(count)
  }

  private def read(input: String): Sudoku = {
    for (ij <- 0 until input.length.min(SIZE * SIZE)) {
      val k = DIGITS.indexOf(input(ij))
      if (k != -1) {
        assign(coord(ij / SIZE, ij % SIZE, k))
      }
    }
    this
  }

  private def search(): Sudoku = {

    val min = count.min
    if (min == DEFINED) {
      return this
    }

    for (c <- children(count.indexOf(min)) if admit(c)) {
      try {
        return new Sudoku(this).assign(c).search()
      } catch {
        case _: NoSolutionException => ()
      }
    }

    throw new NoSolutionException
  }

  private def show(): String = {
    val output = new StringBuilder(SIZE * SIZE)
    for (i <- 0 until SIZE ; j <- 0 until SIZE) {
      val ks = (0 until SIZE).filter { k => admit(coord(i, j, k)) }
      if (ks.size == 1) {
        output += DIGITS(ks(0))
      } else {
        output += '.'
      }
    }
    output.toString
  }

  private def assign(c: Int): Sudoku = {
    val queue = Queue(c)
    while (!queue.isEmpty) {
      val c0 = queue.dequeue()
      if (!admit(c0)) {
        throw new NoSolutionException
      }
      for (b1 <- parents(c0)) {
        count(b1) = DEFINED
        for (c2 <- children(b1) if c2 != c0 && admit(c2)) {
          admit(c2) = false
          for (b3 <- parents(c2) if b3 != b1) {
            count(b3) -= 1
            if (count(b3) == 0) {
              throw new NoSolutionException
            }
            if (count(b3) == 1) {
              for (c4 <- children(b3) if admit(c4)) {
                queue.enqueue(c4)
              }
            }
          }
        }
      }
    }
    this
  }
}
