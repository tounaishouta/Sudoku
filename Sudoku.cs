using System;
using System.Collections.Generic;
using System.Linq;

using Coord = System.Int32;
using Block = System.Int32;

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
        catch
        {
            return "NO SOLUTION";
        }
    }

    private class NoSolutionException : Exception
    {}

    private const int UNIT  = 3;
    private const int SIZE  = UNIT * UNIT;
    private const int GRD   = 0;
    private const int ROW   = 1;
    private const int COL   = 2;
    private const int BOX   = 3;
    private const int VIEW  = 4;
    private const int COORD = SIZE * SIZE * SIZE;
    private const int BLOCK = VIEW * SIZE * SIZE;

    private const int DEFINED = 0xDEF;

    private const string DIGITS = "123456789";

    private static Coord coord(int i, int j, int k) =>
        (i * SIZE + j) * SIZE + k;

    private static Block block(int v, int p, int q) =>
        (v * SIZE + p) * SIZE + q;

    private static List<Block>[] parents  = new List<Block>[COORD];
    private static List<Coord>[] children = new List<Coord>[BLOCK];

    static Sudoku()
    {
        for (var i = 0; i < SIZE; i++)
            for (var j = 0; j < SIZE; j++)
                for (var k = 0; k < SIZE; k++)
                    parents[coord(i, j, k)] = new List<Coord>(new[] {
                            block(GRD, i, j),
                            block(ROW, i, k),
                            block(COL, j, k),
                            block(BOX, i / UNIT * UNIT + j / UNIT, k)});

        for (var b = 0; b < BLOCK; b++)
            children[b] = new List<Coord>(SIZE);
        for (var c = 0; c < COORD; c++)
            foreach (var b in parents[c])
                children[b].Add(c);
    }

    private bool[] admit = new bool[COORD];

    private int[] count = new int[BLOCK];

    private Sudoku()
    {
        for (var c = 0; c < COORD; c++)
            admit[c] = true;
        for (var b = 0; b < BLOCK; b++)
            count[b] = SIZE;
    }

    private Sudoku(Sudoku that)
    {
        for (var c = 0; c < COORD; c++)
            admit[c] = that.admit[c];
        for (var b = 0; b < BLOCK; b++)
            count[b] = that.count[b];
    }

    private Sudoku search()
    {
        var m = count.Min();
        if (m == DEFINED)
            return this;

        var b = Array.IndexOf(count, m);
        foreach (var c in children[b]) if (admit[c])
        {
            try
            {
                return new Sudoku(this).assign(c).search();
            }
            catch (NoSolutionException)
            {}
        }

        throw new NoSolutionException();
    }

    private Sudoku read(string input)
    {
        for (var ij = 0; ij < Math.Min(SIZE * SIZE, input.Length); ij++)
        {
            var k = DIGITS.IndexOf(input[ij]);
            if (k != -1)
                assign(coord(ij / SIZE, ij % SIZE, k));
        }
        return this;
    }

    public override string ToString()
    {
        var output = new char[SIZE * SIZE];
        for (var i = 0; i < SIZE; i++)
        {
            for (var j = 0; j < SIZE; j++)
            {
                var ij = i * SIZE + j;
                var ks = new List<int>(SIZE);
                for (var k = 0; k < SIZE; k++) if (admit[coord(i, j, k)])
                    ks.Add(k);
                if (ks.Count == 1)
                    output[ij] = DIGITS[ks[0]];
                else
                    output[ij] = '.';
            }
        }
        return new string(output);
    }

    private Sudoku assign(Coord c)
    {
        var queue = new Queue<Coord>();
        queue.Enqueue(c);
        while (queue.Count > 0)
        {
            var c0 = queue.Dequeue();
            if (!admit[c0])
                throw new NoSolutionException();
            foreach (var b1 in parents[c0])
            {
                count[b1] = DEFINED;
                foreach (var c2 in children[b1]) if (c2 != c0 && admit[c2])
                {
                    admit[c2] = false;
                    foreach (var b3 in parents[c2]) if (b3 != b1)
                    {
                        count[b3]--;
                        if (count[b3] == 0)
                            throw new NoSolutionException();
                        if (count[b3] == 1)
                            foreach (var c4 in children[b3]) if (admit[c4])
                                queue.Enqueue(c4);
                    }
                }
            }
        }
        return this;
    }
}
