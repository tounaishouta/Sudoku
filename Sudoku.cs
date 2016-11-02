using System;
using System.Collections.Generic;
using System.Linq;

using Coord = System.Int32;
using Block = System.Int32;
using Count = System.Int32;

using static View;
using static State;

enum View { GRD, ROW, COL, BOX };
enum State { OPEN, FIXED, BANNED };

class Sudoku
{
    public static void Main() {
        string input;
        while ((input = Console.ReadLine()) != null)
            Console.WriteLine(Solve(input));
    }

    public static string Solve(string input)
    {
        try
        {
            return new Sudoku().read(input).search().ToString();
        }
        catch (NoSolutionException)
        {
            return "NO SOLUTION";
        }
    }

    private class NoSolutionException : Exception
    {}

    private const int VIEW  = 4;
    private const int UNIT  = 3;
    private const int SIZE  = UNIT * UNIT;
    private const int COORD = SIZE * SIZE * SIZE;
    private const int BLOCK = VIEW * SIZE * SIZE;

    private const Count DONE = SIZE + 1;

    private const string DIGITS = "123456789";

    private static Coord coord(int i, int j, int k) =>
        (i * SIZE + j) * SIZE + k;

    private static Block block(View v, int p, int q) =>
        ((int)v * SIZE + p) * SIZE + q;

    private static List<Block>[] parents  = new List<Block>[COORD];
    private static List<Coord>[] children = new List<Coord>[BLOCK];

    static Sudoku()
    {
        for (var i = 0; i < SIZE; i++)
            for (var j = 0; j < SIZE; j++)
            {
                var p = i / UNIT * UNIT + j / UNIT;
                for (var k = 0; k < SIZE; k++)
                    parents[coord(i, j, k)] = new List<Coord>(new[] {
                            block(View.GRD, i, j),
                            block(ROW, i, k),
                            block(COL, j, k),
                            block(BOX, p, k),
                            });
            }

        for (var b = 0; b < BLOCK; b++)
            children[b] = new List<Coord>(SIZE);
        for (var c = 0; c < COORD; c++)
            foreach (var b in parents[c])
                children[b].Add(c);
    }

    private State[]      state = new State[COORD];
    private Count[]      count = new Count[BLOCK];
    private Queue<Coord> queue = new Queue<Coord>();

    private Sudoku()
    {
        for (var c = 0; c < COORD; c++)
            state[c] = OPEN;
        for (var b = 0; b < BLOCK; b++)
            count[b] = SIZE;
    }

    private Sudoku(Sudoku that)
    {
        for (var c = 0; c < COORD; c++)
            state[c] = that.state[c];
        for (var b = 0; b < BLOCK; b++)
            count[b] = that.count[b];
    }

    private Sudoku fix(Coord c)
    {
        queue.Enqueue(c);
        return this;
    }

    private Sudoku read(string input)
    {
        var len = input.Length;
        for (var i = 0; i < SIZE; i++)
            for (var j = 0; j < SIZE; j++)
                if (i * SIZE + j < len)
                {
                    var k = DIGITS.IndexOf(input[i * SIZE + j]);
                    if (k != -1)
                        fix(coord(i, j, k));
                }
        return this;
    }

    private Sudoku search()
    {
        while (queue.Count > 0)
        {
            var c0 = queue.Dequeue();
            if (state[c0] == FIXED)
                continue;
            if (state[c0] == BANNED)
                throw new NoSolutionException();
            state[c0] = FIXED;
            foreach (var b1 in parents[c0])
            {
                count[b1] = DONE;
                foreach (var c2 in children[b1])
                    if (c2 != c0 && state[c2] == OPEN)
                    {
                        state[c2] = BANNED;
                        foreach (var b3 in parents[c2])
                            if (b3 != b1)
                            {
                                count[b3]--;
                                if (count[b3] == 0)
                                    throw new NoSolutionException();
                                if (count[b3] == 1)
                                    foreach (var c4 in children[b3])
                                        if (state[c4] == OPEN)
                                            fix(c4);
                            }
                    }
            }
        }

        var m = count.Min();
        if (m == DONE)
            return this;

        var b = Array.IndexOf(count, m);
        foreach (var c in children[b])
            if (state[c] == OPEN)
            {
                try
                {
                    return new Sudoku(this).fix(c).search();
                }
                catch (NoSolutionException)
                {}
            }

        throw new NoSolutionException();
    }

    public override string ToString()
    {
        var output = new char[SIZE * SIZE];
        for (var ij = 0; ij < SIZE; ij++)
            output[ij] = '.';
        for (var i = 0; i < SIZE; i++)
            for (var j = 0; j < SIZE; j++)
                for (var k = 0; k < SIZE; k++)
                    if (state[coord(i, j, k)] == FIXED)
                        output[i * SIZE + j] = DIGITS[k];
        return new string(output);
    }
}
