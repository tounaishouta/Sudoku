import java.util.LinkedList

fun main(args: Array<String>) {
    while (true) {
        val input = readLine()
        if (input == null) {
            return
        }
        println(Sudoku.solve(input) ?: "no solution")
    }
}

private val size    = 3
private val nDigit  = size * size
private val digits  = 0 .. nDigit - 1
private val nCoord  = nDigit * nDigit * nDigit
private val GRD     = 0
private val ROW     = 1
private val COL     = 2
private val BOX     = 3
private val nView   = 4
private val nBlock  = nView * nDigit * nDigit
private val defined = 0xDEF

private fun coordOf(i: Int, j: Int, k: Int): Int = (i * nDigit + j) * nDigit + k
private fun blockOf(v: Int, i: Int, j: Int): Int = (v * nDigit + i) * nDigit + j

private val parents = Array(nCoord) { c ->
    val i = c / nDigit / nDigit
    val j = c / nDigit % nDigit
    val k = c % nDigit
    val p = i / size * size + j / size
    listOf(blockOf(GRD, i, j), blockOf(ROW, i, k), blockOf(COL, j, k), blockOf(BOX, p, k))
}
private val children = Array(nBlock) { LinkedList<Int>() } .apply {
    parents.forEachIndexed { c, bs -> bs.forEach { b -> get(b).add(c) } }
}
private val siblings = Array(nCoord) { c ->
    LinkedList<Int>().apply { parents[c].forEach { b -> children[b].forEach { if (it != c) add(it) } } } .toSortedSet().toList()
}

private fun Char.toDigit(): Int? = if (this in '1' .. '9') this.toInt() - '1'.toInt() else null
private fun Int.fromDigit(): Char = ('1'.toInt() + this).toChar()

private fun <T, R> Iterable<T>.flatMapNullable(f: (T) -> R?): R? {
    for (item in this) {
        val result = f(item)
        if (result != null) {
            return result
        }
    }
    return null
}

class Sudoku() {

    constructor(that: Sudoku): this() {
        that.admits.forEachIndexed { i, a -> admits[i] = a }
        that.option.forEachIndexed { i, o -> option[i] = o }
    }

    private val admits = BooleanArray(nCoord) { true }
    private val option = IntArray(nBlock) { nDigit }

    fun read(input: String): Sudoku? = input.indices.fold<Int, Sudoku?>(this) { s, p ->
        val i = p / nDigit
        val j = p % nDigit
        val k = input[p].toDigit()
        if (k == null) s
        else s?.assign(coordOf(i, j, k))
    }

    fun solve(): Sudoku? = option.min()!!.let { m ->
        if (m == defined) this else children[option.indexOf(m)].flatMapNullable { Sudoku(this).assign(it)?.solve() }
    }

    private fun assign(c: Int): Sudoku? {
        if (! admits[c]) {
            return null
        }
        val cs = siblings[c].filter { admits[it] }
        val bs = cs.flatMap { parents[it] }
        cs.forEach { admits[it] = false }
        bs.forEach { option[it]-- }
        parents[c].forEach { option[it] = defined }
        return bs.toSortedSet().fold<Int, Sudoku?>(this) { s, b -> s?.check(b) }
    }

    private fun check(b: Int): Sudoku? = when (option[b]) {
        0    -> null
        1    -> assign(children[b].find { admits[it] } !!)
        else -> this
    }

    override fun toString(): String = StringBuilder().apply { digits.forEach { i -> digits.forEach { j -> append(toChar(i, j)) } } }.toString()

    private fun toChar(i: Int, j: Int): Char = if (option[blockOf(GRD, i, j)] == defined) digits.find { admits[coordOf(i, j, it)] } !!.fromDigit() else '.'

    companion object {
        fun solve(input: String): Sudoku? = Sudoku().read(input)?.solve()
    }
}
