fun main(args: Array<String>) {
    while (true) {
        println(Sudoku.solve(readLine() ?: return))
    }
}

class Sudoku {

    enum class View { GRD, ROW, COL, BOX }

    enum class State { OPEN, FIXED, BANNED }

    class NoChoice : Exception("NoChoice");

    companion object {

        const val UNIT  = 3
        const val SIZE  = UNIT * UNIT
        const val COORD = SIZE * SIZE * SIZE
        const val BLOCK = 4 * SIZE * SIZE
        const val DONE  = SIZE + 1

        const val DIGITS = "123456789"

        fun coord(i: Int, j: Int, k: Int): Int = (i * SIZE + j) * SIZE + k

        fun block(v: View, p: Int, q: Int): Int = when (v) {
            View.GRD -> (0 * SIZE + p) * SIZE + q
            View.ROW -> (1 * SIZE + p) * SIZE + q
            View.COL -> (2 * SIZE + p) * SIZE + q
            View.BOX -> (3 * SIZE + p) * SIZE + q
        }

        val parents = Array(COORD) { mutableListOf<Int>() } .apply {
            for (i in 0 until SIZE) {
                for (j in 0 until SIZE) {
                    val p = i / UNIT * UNIT + j / UNIT
                    for (k in 0 until SIZE) {
                        get(coord(i, j, k)).apply {
                            add(block(View.GRD, i, j))
                            add(block(View.ROW, i, k))
                            add(block(View.COL, j, k))
                            add(block(View.BOX, p, k))
                        }
                    }
                }
            }
        }

        val children = Array(BLOCK) { mutableListOf<Int>() } .apply {
            for (c in 0 until COORD)
                for (b in parents[c])
                    get(b).add(c)
        }

        fun solve(input: String): String {
            return Sudoku().read(input).search()?.show() ?: "NO SOLUTION"
        }
    }

    val state: Array<State>
    val count: IntArray
    val queue: MutableList<Int>

    constructor() {
        state = Array<State>(COORD) { State.OPEN }
        count = IntArray(BLOCK) { SIZE }
        queue = mutableListOf<Int>()
    }

    constructor(that: Sudoku) {
        state = that.state.clone()
        count = that.count.clone()
        queue = mutableListOf<Int>()
    }

    fun enqueue(c: Int): Sudoku {
        queue.add(c)
        return this
    }

    fun read(input: String): Sudoku {
        val len = input.length
        for (i in 0 until SIZE) {
            for (j in 0 until SIZE) {
                if (i * SIZE + j < len) {
                    val k = DIGITS.indexOf(input[i * SIZE + j])
                    if (k != -1)
                        enqueue(coord(i, j, k))
                }
            }
        }
        return this
    }

    fun show(): String {
        val output = CharArray(SIZE * SIZE) { '.' }
        for (i in 0 until SIZE)
            for (j in 0 until SIZE)
                for (k in 0 until SIZE)
                    if (state[coord(i, j, k)] == State.FIXED)
                        output[i * SIZE + j] = DIGITS[k]
        return String(output)
    }

    fun search(): Sudoku? {

        while (!queue.isEmpty()) {
            val c = queue.removeAt(0)
            when (state[c]) {
                State.OPEN ->
                    try { fix(c) } catch (_: NoChoice) { return null }
                State.FIXED ->
                    Unit
                State.BANNED ->
                    return null
            }
        }

        val m = count.min()!!
        if (m == DONE)
            return this

        val b = count.indexOf(m)
        for (c in children[b])
            if (state[c] == State.OPEN)
                Sudoku(this).enqueue(c).search()?.let { return it }

        return null
    }

    fun fix(c: Int) {
        state[c] = State.FIXED
        for (b in parents[c])
            mark(b)
    }

    fun mark(b: Int) {
        count[b] = DONE
        for (c in children[b])
            if (state[c] == State.OPEN)
                ban(c)
    }

    fun ban(c: Int) {
        state[c] = State.BANNED
        for (b in parents[c])
            if (count[b] != DONE)
                countdown(b)
    }

    fun countdown(b: Int) {
        count[b]--
        when (count[b]) {
            0 -> throw NoChoice();
            1 ->
                for (c in children[b])
                    if (state[c] == State.OPEN)
                        enqueue(c);
            else -> Unit
        }
    }
}
