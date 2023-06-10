using System;
using System.Collections.Generic;
using System.Linq;

using Coord = System.Int32;
using Block = System.Int32;
using Count = System.Int32;

class Sudoku
{
    static void Main()
    {
        string input;
        while ((input = Console.ReadLine()) != null)
            Console.WriteLine(Solve(input));
    }

    static string Solve(string input)
    {
        try
        {
            return new Sudoku().read(input).search().show();
        }
        catch (NoSolutionException)
        {
            return "NO SOLUTION";
        }
    }

    class NoSolutionException : Exception {}

    enum View { GRD, ROW, COL, BOX }
    enum State { OPEN, FIXED, BANNED }

    const int UNIT = 3;
    const int SIZE = UNIT * UNIT;
    const int COORD = SIZE * SIZE * SIZE;
    const int BLOCK = 4 * SIZE * SIZE;
    const Count DONE = SIZE + 1;
    const string DIGITS = "123456789";

    static Coord coord(int i, int j, int k) =>
        (i * SIZE + j) * SIZE + k;

    static Block block(View v, int p, int q) =>
        ((int)v * SIZE + p) * SIZE + q;

    static List<Block>[] parents = new List<Block>[COORD];
    static List<Coord>[] children = new List<Coord>[BLOCK];

    static Sudoku() {
        for (int i = 0; i < SIZE; i++)
            for (int j = 0; j < SIZE; j++)
            {
                int p = i / UNIT * UNIT + j / UNIT;
                for (int k = 0; k < SIZE; k++)
                {
                    var list = new List<Block>(4);
                    list.Add(block(View.GRD, i, j));
                    list.Add(block(View.ROW, i, k));
                    list.Add(block(View.COL, j, k));
                    list.Add(block(View.BOX, p, k));
                    parents[coord(i, j, k)] = list;
                }
            }
        for (Block b = 0; b < BLOCK; b++)
            children[b] = new List<Coord>(SIZE);
        for (Coord c = 0; c < COORD; c++)
            foreach (Block b in parents[c])
                children[b].Add(c);
    }

    State[] state = new State[COORD];
    Count[] count = new Count[BLOCK];
    Queue<Coord> queue = new Queue<Coord>();

    Sudoku()
    {
        for (Coord c = 0; c < COORD; c++)
            state[c] = State.OPEN;
        for (Block b = 0; b < BLOCK; b++)
            count[b] = SIZE;
    }

    Sudoku(Sudoku that)
    {
        for (Coord c = 0; c < COORD; c++)
            state[c] = that.state[c];
        for (Block b = 0; b < BLOCK; b++)
            count[b] = that.count[b];
    }

    Sudoku enqueue(Coord c)
    {
        queue.Enqueue(c);
        return this;
    }

    Sudoku read(string input)
    {
        int len = input.Length;
        for (int i = 0; i < SIZE; i++)
            for (int j = 0; j < SIZE; j++)
                if (i * SIZE + j < len)
                {
                    int k = DIGITS.IndexOf(input[i * SIZE + j]);
                    if (k != -1)
                        enqueue(coord(i, j, k));
                }
        return this;
    }

    string show()
    {
        char[] result = new char[SIZE * SIZE];
        for (int i = 0; i < SIZE; i++)
            for (int j = 0; j < SIZE; j++)
            {
                result[i * SIZE + j] = '.';
                for (int k = 0; k < SIZE; k++)
                    if (state[coord(i, j, k)] == State.FIXED)
                        result[i * SIZE + j] = DIGITS[k];
            }
        return new string(result);
    }

    Sudoku search()
    {
        while (queue.Count > 0)
        {
            Coord c = queue.Dequeue();
            if (state[c] == State.FIXED)
                continue;
            if (state[c] == State.BANNED)
                throw new NoSolutionException();
            fix(c);
        }

        Count min = count.Min();
        if (min == DONE)
            return this;

        Block b = Array.IndexOf(count, min);
        foreach (Coord c in children[b])
            if (state[c] == State.OPEN)
            {
                try
                {
                    return new Sudoku(this).enqueue(c).search();
                }
                catch (NoSolutionException)
                {}
            }

        throw new NoSolutionException();
    }

    void fix(Coord c)
    {
        state[c] = State.FIXED;
        foreach (Block b in parents[c])
            mark_done(b);
    }

    void mark_done(Block b)
    {
        count[b] = DONE;
        foreach (Coord c in children[b])
            if (state[c] == State.OPEN)
                ban(c);
    }

    void ban(Coord c)
    {
        state[c] = State.BANNED;
        foreach (Block b in parents[c])
            if (count[b] != DONE)
                countdown(b);
    }

    void countdown(Block b)
    {
        count[b]--;
        if (count[b] == 0)
            throw new NoSolutionException();
        if (count[b] == 1)
            foreach (Coord c in children[b])
                if (state[c] == State.OPEN)
                    enqueue(c);
    }
}
