fun main(args: Array<String>) {
    while (true) {
        println(Sudoku.solve(readLine() ?: return))
    }
}

class Sudoku {

    companion object {

        const val UNIT  = 3
        const val GRD   = 0
        const val ROW   = 1
        const val COL   = 2
        const val BOX   = 3
        const val VIEW  = 4
        const val SIZE  = UNIT * UNIT
        const val COORD = SIZE * SIZE * SIZE
        const val BLOCK = VIEW * SIZE * SIZE

        const val OPEN   = 0
        const val FIXED  = 1
        const val BANNED = 2

        const val DONE = SIZE + 1

        const val DIGITS = "123456789"

        fun coord(i: Int, j: Int, k: Int): Int = (i * SIZE + j) * SIZE + k

        fun block(v: Int, p: Int, q: Int): Int = (v * SIZE + p) * SIZE + q

        val parents = Array(COORD) { mutableListOf<Int>() } .apply {
            for (i in 0 until SIZE) {
                for (j in 0 until SIZE) {
                    val p = i / UNIT * UNIT + j / UNIT
                    for (k in 0 until SIZE) {
                        get(coord(i, j, k)).apply {
                            add(block(GRD, i, j))
                            add(block(ROW, i, k))
                            add(block(COL, j, k))
                            add(block(BOX, p, k))
                        }
                    }
                }
            }
        }

        val children = Array(BLOCK) { mutableListOf<Int>() } .apply {
            for (c in 0 until COORD) {
                for (b in parents[c]) {
                    get(b).add(c)
                }
            }
        }

        fun solve(input: String): String {
            return Sudoku().read(input).search()?.show() ?: "NO SOLUTION"
        }
    }

    val state: IntArray
    val count: IntArray
    val queue: MutableList<Int>

    constructor() {
        state = IntArray(COORD) { OPEN }
        count = IntArray(BLOCK) { SIZE }
        queue = mutableListOf<Int>()
    }

    constructor(that: Sudoku) {
        state = that.state.clone()
        count = that.count.clone()
        queue = mutableListOf<Int>()
    }

    fun fix(c: Int): Sudoku {
        queue.add(c)
        return this
    }

    fun read(input: String): Sudoku {
        val len = input.length
        for (i in 0 until SIZE) {
            for (j in 0 until SIZE) {
                if (i * SIZE + j < len) {
                    val k = DIGITS.indexOf(input[i * SIZE + j])
                    if (k != -1) {
                        fix(coord(i, j, k))
                    }
                }
            }
        }
        return this
    }

    fun show(): String {
        val output = CharArray(SIZE * SIZE) { '.' }
        for (i in 0 until SIZE) {
            for (j in 0 until SIZE) {
                for (k in 0 until SIZE) {
                    if (state[coord(i, j, k)] == FIXED) {
                        output[i * SIZE + j] = DIGITS[k]
                    }
                }
            }
        }
        return String(output)
    }

    fun search(): Sudoku? {

        while (!queue.isEmpty()) {
            val c0 = queue.removeAt(0)
            if (state[c0] == FIXED) {
                continue
            }
            if (state[c0] == BANNED) {
                return null
            }
            state[c0] = FIXED
            for (b1 in parents[c0]) {
                count[b1] = DONE
                for (c2 in children[b1]) {
                    if (c2 != c0 && state[c2] == OPEN) {
                        state[c2] = BANNED
                        for (b3 in parents[c2]) {
                            if (b3 != b1) {
                                count[b3]--
                                if (count[b3] == 0) {
                                    return null
                                }
                                if (count[b3] == 1) {
                                    for (c4 in children[b3]) {
                                        if (state[c4] == OPEN) {
                                            fix(c4)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        val m = count.min()!!
        if (m == DONE) {
            return this
        }

        val b = count.indexOf(m)
        for (c in children[b]) {
            if (state[c] == OPEN) {
                Sudoku(this).fix(c).search()?.let {
                    return it
                }
            }
        }

        return null
    }
}
