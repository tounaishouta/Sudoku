fun main(args: Array<String>) {
    while (true) {
        println(Sudoku.solve(readLine() ?: return))
    }
}

class Sudoku private constructor() {

    companion object {

        fun solve(input: String): String = Sudoku().read(input)?.search()?.show() ?: "NO SOLUTION"

        private val GRD     = 0
        private val ROW     = 1
        private val COL     = 2
        private val BOX     = 3
        private val VIEW    = 4
        private val UNIT    = 3
        private val SIZE    = UNIT * UNIT
        private val DIGITS  = "123456789"
        private val COORD   = SIZE * SIZE * SIZE
        private val BLOCK   = VIEW * SIZE * SIZE
        private val DEFINED = 0xDEF

        private fun coordOf(i: Int, j: Int, k: Int): Int = (i * SIZE + j) * SIZE + k
        private fun blockOf(v: Int, p: Int, q: Int): Int = (v * SIZE + p) * SIZE + q

        private val parents = Array(COORD) { c ->
            val i = c / SIZE / SIZE
            val j = c / SIZE % SIZE
            val k = c % SIZE
            val p = i / UNIT * UNIT + j / UNIT
            listOf(blockOf(GRD, i, j), blockOf(ROW, i, k), blockOf(COL, j, k), blockOf(BOX, p, k))
        }

        private val children = Array(BLOCK) { mutableListOf<Int>() }.apply {
            for ((c, bs) in parents.withIndex()) {
                for (b in bs) {
                    get(b).add(c)
                }
            }
        }
    }

    private val admit = BooleanArray(COORD) { true }
    private val count = IntArray(BLOCK) { SIZE }

    private constructor(that: Sudoku) : this() {
        for ((c, a) in that.admit.withIndex()) {
            admit[c] = a
        }
        for ((b, n) in that.count.withIndex()) {
            count[b] = n
        }
    }

    private fun read(input: String): Sudoku? {
        for (ij in 0 until Math.min(SIZE * SIZE, input.length)) {
            val k = DIGITS.indexOf(input[ij])
            if (k != -1) {
                assign(coordOf(ij / SIZE, ij % SIZE, k)) ?: return null
            }
        }
        return this
    }

    private fun search(): Sudoku? {
        val m = count.min()!!
        if (m == DEFINED) {
            return this
        }
        val b = count.indexOf(m)
        for (c in children[b]) if (admit[c]) {
            Sudoku(this).assign(c)?.search()?.let { that ->
                return that
            }
        }
        return null
    }

    private fun show(): String = StringBuilder().apply {
        for (i in 0 until SIZE) for (j in 0 until SIZE) {
            val ks = (0 until SIZE).filter { k -> admit[coordOf(i, j, k)] }
            append(if (ks.size == 1) DIGITS[ks.get(0)] else '.')
        }
    }.toString()

    private fun assign(c: Int): Sudoku? {
        val queue = mutableListOf(c)
        while (queue.size > 0) {
            val c0 = queue.removeAt(0)
            if (!admit[c0]) {
                return null
            }
            for (b1 in parents[c0]) {
                count[b1] = DEFINED
                for (c2 in children[b1]) if (c2 != c0 && admit[c2]) {
                    admit[c2] = false
                    for (b3 in parents[c2]) if (b3 != b1) {
                        count[b3]--
                        if (count[b3] == 0) {
                            return null
                        }
                        if (count[b3] == 1) {
                            for (c4 in children[b3]) if (admit[c4]) {
                                queue.add(c4)
                            }
                        }
                    }
                }
            }
        }
        return this
    }
}
