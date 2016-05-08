using System;
using System.Collections.Generic;
using System.Linq;

namespace Sudoku
{
    public class Sudoku
    {
        public static void Main()
        {
            string input;
            while ((input = Console.ReadLine()) != null)
                Console.WriteLine(Solve(input));
        }

        public static string Solve(string input)
        {
            try
            {
                var s = new Sudoku();
                s.read(input);
                return s.search().ToString();
            }
            catch (NoSolutionException)
            {
                return "NO SOLUTION";
            }
        }

        private class NoSolutionException : Exception
        {
        }

        private const int M = 3;
        private const int N = M * M;
        private const int V = 4;

        private const int GRD = 0;
        private const int ROW = 1;
        private const int COL = 2;
        private const int BOX = 3;

        private const int COORD = N * N * N;
        private const int BLOCK = V * N * N;

        private const int DEFINED = 0xDEF;

        private const string DIGITS = "123456789";

        private static List<int>[] parents  = new List<int>[COORD];
        private static List<int>[] children = new List<int>[BLOCK];
        private static List<int>[] siblings = new List<int>[COORD];

        private static int coord(int i, int j, int k) => (i * N + j) * N + k;
        private static int block(int i, int j, int k) => (i * N + j) * N + k;

        static Sudoku()
        {
            for (var i = 0; i < N; i++)
                for (var j = 0; j < N; j++)
                    for (var k = 0; k < N; k++)
                        parents[coord(i, j, k)] = new List<int>(new[]
                                { block(GRD, i, j)
                                , block(ROW, i, k)
                                , block(COL, j, k)
                                , block(BOX, i / M * M + j / M, k)
                                });

            for (var b = 0; b < BLOCK; b++)
                children[b] = new List<int>(V);
            for (var c = 0; c < COORD; c++)
                foreach (var b in parents[c])
                    children[b].Add(c);

            for (var c = 0; c < COORD; c++)
            {
                var ccs = new SortedSet<int>();
                foreach (var b in parents[c])
                    foreach (var cc in children[b])
                        ccs.Add(cc);
                ccs.Remove(c);
                siblings[c] = ccs.ToList();
            }
        }

        private bool[] admits = new bool[COORD];

        private int[] rest = new int[BLOCK];

        private Sudoku()
        {
            for (var c = 0; c < COORD; c++)
                admits[c] = true;
            for (var b = 0; b < BLOCK; b++)
                rest[b] = N;
        }

        private Sudoku(Sudoku that)
        {
            that.admits.CopyTo(admits, 0);
            that.rest.CopyTo(rest, 0);
        }

        private Sudoku search()
        {
            var m = rest.Min();
            if (m == DEFINED)
                return this;

            var b = Array.IndexOf(rest, m);
            foreach (var c in children[b])
            {
                try
                {
                    var that = new Sudoku(this);
                    that.assign(c);
                    return that.search();
                }
                catch (NoSolutionException)
                {
                }
            }

            throw new NoSolutionException();
        }

        private void read(string input)
        {
            var l = input.Length;
            for (var i = 0; i < N && i * N < l; i++)
            {
                for (var j = 0; i < N && i * N + j < l; j++)
                {
                    var k = DIGITS.IndexOf(input[i * N + j]);
                    if (k != -1)
                        assign(coord(i, j, k));
                }
            }
        }

        private void assign(int c)
        {
            if (!admits[c])
                throw new NoSolutionException();

            var bbs = new List<int>();
            foreach (var cc in siblings[c])
            {
                if (admits[cc])
                {
                    admits[cc] = false;
                    foreach (var bb in parents[cc])
                    {
                        rest[bb]--;
                        bbs.Add(bb);
                    }
                }
            }

            foreach (var b in parents[c])
                rest[b] = DEFINED;

            foreach (var bb in bbs)
                check(bb);
        }

        private void check(int b)
        {
            if (rest[b] == 0)
                throw new NoSolutionException();

            if (rest[b] == 1)
                foreach (var c in children[b])
                    if (admits[c])
                        assign(c);
        }

        public override string ToString()
        {
            var output = new char[N * N];
            for (var i = 0; i < N; i++)
            {
                for (var j = 0; j < N; j++)
                {
                    var count = 0;
                    for (var k = 0; k < N; k++)
                    {
                        if (admits[coord(i, j, k)])
                        {
                            output[i * N + j] = DIGITS[k];
                            count++;
                        }
                    }
                    if (count != 1)
                        output[i * N + j] = '.';
                }
            }
            return new string(output);
        }
    }
}
