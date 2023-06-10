void main()
{
    import std.stdio: readln, writeln;
    import std.string: chomp;

    Sudoku.init();
    string line;
    while ((line = readln()) !is null)
        writeln(Sudoku.solve(chomp(line)));
}

class Sudoku
{
    static string solve(string input)
    {
        try { return new Sudoku().read(input).search().show(); }
        catch (NoSolutionException) { return "NO SOLUTION"; }
    }

    class NoSolutionException : Exception
    {
        this () { super("NO SOLUTION"); }
    }

    enum View { grd, row, col, box }
    enum State { open, fixed, banned }

    alias Coord = int;
    alias Block = int;
    alias Count = int;

    static immutable int unit = 3;
    static immutable int size = unit * unit;
    static immutable int nCoord = size * size * size;
    static immutable int nBlock = 4 * size * size;
    static immutable Count done = size + 1;
    static immutable string digits = "123456789";

    static Block[][nCoord] parents;
    static Coord[][nBlock] children;

    static Coord coord(int i, int j, int k)
    {
        return (i * size + j) * size + k;
    }

    static Block block(View v, int p, int q)
    {
        return (cast(int) v * size + p) * size + q;
    }

    static void init()
    {
        for (int i = 0; i < size; i++)
            for (int j = 0; j < size; j++)
            {
                int p = i / unit * unit + j / unit;
                for (int k = 0; k < size; k++)
                    parents[coord(i, j, k)] = [
                        block(View.grd, i, j),
                        block(View.row, i, k),
                        block(View.col, j, k),
                        block(View.box, p, k),
                    ];
            }

        for (Coord c = 0; c < nCoord; c++)
            foreach (Block b; parents[c])
                children[b] ~= c;
    }

    State[nCoord] state;
    Count[nBlock] count;
    Coord[] queue;

    this()
    {
        state[] = State.open;
        count[] = size;
    }

    this(const Sudoku that)
    {
        state[] = that.state[];
        count[] = that.count[];
    }

    Sudoku enqueue(Coord c)
    {
        queue ~= c;
        return this;
    }

    Sudoku read(string input)
    {
        import std.string: indexOf;

        for (int i = 0; i < size; i++)
            for (int j = 0; j < size; j++)
                if (i * size + j < input.length)
                {
                    int k = cast(int) indexOf(digits, input[i * size + j]);
                    if (k != -1)
                        enqueue(coord(i, j, k));
                }
        return this;
    }

    string show()
    {
        import std.conv: to;

        char[size * size] result;
        result[] = '.';
        for (int i = 0; i < size; i++)
            for (int j = 0; j < size; j++)
                for (int k = 0; k < size; k++)
                    if (state[coord(i, j, k)] == State.fixed)
                        result[i * size + j] = digits[k];
        return to!string(result);
    }

    Sudoku search()
    {
        while (queue.length > 0)
        {
            Coord c = queue[0];
            queue = queue[1..$];
            if (state[c] == State.open)
                fix(c);
            if (state[c] == State.fixed)
                continue;
            if (state[c] == State.banned)
                throw new NoSolutionException;
        }

        Count min = done;
        Block bmin;
        for (Block b = 0; b < nBlock; b++)
            if (count[b] < min)
            {
                min = count[b];
                bmin = b;
            }

        if (min == done)
            return this;

        foreach (Coord c; children[bmin])
            if (state[c] == State.open)
            {
                try { return new Sudoku(this).enqueue(c).search(); }
                catch (NoSolutionException) {}
            }

        throw new NoSolutionException;
    }

    void fix(Coord c)
    {
        state[c] = State.fixed;
        foreach (Block b; parents[c])
            mark_done(b);
    }

    void mark_done(Block b)
    {
        count[b] = done;
        foreach (Coord c; children[b])
            if (state[c] == State.open)
                ban(c);
    }

    void ban(Coord c)
    {
        state[c] = State.banned;
        foreach (Block b; parents[c])
            if (count[b] != done)
                countdown(b);
    }

    void countdown(Block b)
    {
        count[b]--;
        if (count[b] == 0)
            throw new NoSolutionException;
        if (count[b] == 1)
            foreach (Coord c; children[b])
                if (state[c] == State.open)
                    enqueue(c);
    }
}
