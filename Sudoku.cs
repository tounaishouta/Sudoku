using System;
using System.Collections.Generic;
namespace Sudoku {
    public class Sudoku {

        const int m = 3;
        const int n = m * m;

        const String digits = "123456789";

        static LinkedList<int>[] parents;
        static LinkedList<int>[] children;
        static LinkedList<int>[] siblings;

        static int coord(int i, int j, int k) {
            return (i * n + j) * n + k;
        }

        static int block(int v, int p, int q) {
            return (v * n + p) * n + q;
        }

        static Sudoku() {

            parents  = new LinkedList<int>[n * n * n];
            for (var i = 0; i < n; i++) {
                for (var j = 0; j < n; j++) {
                    var p = i / m * m + j / m;
                    for (var k = 0; k < n; k++) {
                        var bs = new LinkedList<int>();
                        bs.AddLast(block(0, i, j));
                        bs.AddLast(block(1, i, k));
                        bs.AddLast(block(2, j, k));
                        bs.AddLast(block(3, p, k));
                        parents[coord(i, j, k)] = bs;
                    }
                }
            }

            children = new LinkedList<int>[4 * n * n];
            for (var b = 0; b < children.Length; b++) {
                children[b] = new LinkedList<int>();
            }
            for (var c = 0; c < parents.Length; c++) {
                foreach (var b in parents[c]) {
                    children[b].AddLast(c);
                }
            }

            siblings = new LinkedList<int>[n * n * n];
            for (var c = 0; c < siblings.Length; c++) {

                var ds = new HashSet<int>();
                foreach (var b in parents[c]) {
                    foreach (var d in children[b]) {
                        ds.Add(d);
                    }
                }
                ds.Remove(c);

                siblings[c] = new LinkedList<int>();
                foreach (var d in ds) {
                    siblings[c].AddLast(d);
                }
            }
        }

        static Sudoku solve(String input) {
            if (input.Length < n * n) {
                Console.WriteLine("Too short input.");
                return null;
            }
            var s = new Sudoku();
            for (var i = 0; i < n; i++) {
                for (var j = 0; j < n; j++) {
                    var k = digits.IndexOf(input[i * n + j]);
                    if (k != -1) {
                        s.assign.AddLast(coord(i, j, k));
                    }
                }
            }
            return s.search();
        }

        static void Main(String[] args) {

            if (args.Length != 1) {
                Console.WriteLine("Usage: mono Sudoku.exe [path]");
                return;
            }

            var file = new System.IO.StreamReader(args[0]);
            while (true) {
                var input = file.ReadLine();
                if (input == null) {
                    return;
                }
                var s = solve(input);
                if (s == null) {
                    Console.WriteLine("NO SOLUTION");
                }
                else {
                    Console.WriteLine(s);
                }
            }
        }

        bool[]          admits;
        int[]           rest;
        LinkedList<int> assign;

        Sudoku() {

            admits = new bool[n * n * n];
            for (var c = 0; c < admits.Length; c++) {
                admits[c] = true;
            }

            rest   = new int[4 * n * n];
            for (var b = 0; b < rest.Length; b++) {
                rest[b] = n;
            }

            assign = new LinkedList<int>();
        }

        Sudoku clone() {
            var s = new Sudoku();
            admits.CopyTo(s.admits, 0);
            rest.CopyTo(s.rest, 0);
            return s;
        }

        Sudoku search() {

            var forbid = new LinkedList<int>();
            var reduce = new LinkedList<int>();
            while (assign.Count > 0) {

                foreach (var c in assign) {
                    if (!admits[c]) {
                        return null;
                    }
                    foreach (var d in siblings[c]) {
                        forbid.AddLast(d);
                    }
                }
                assign.Clear();

                foreach (var c in forbid) {
                    if (admits[c]) {
                        admits[c] = false;
                        foreach (var b in parents[c]) {
                            reduce.AddLast(b);
                        }
                    }
                }
                forbid.Clear();

                foreach (var b in reduce) {
                    rest[b]--;
                    if (rest[b] == 0) {
                        return null;
                    }
                    if (rest[b] == 1) {
                        foreach (var c in children[b]) {
                            if (admits[c]) {
                                assign.AddLast(c);
                            }
                        }
                    }
                }
                reduce.Clear();
            }

            {
                var b = -1;
                var min = n + 1;
                for (var d = 0; d < rest.Length; d++) {
                    if (rest[d] > 1 && rest[d] < min) {
                        min = rest[d];
                        b = d;
                    }
                }

                if (b == -1) {
                    return this;
                }

                foreach (var c in children[b]) {
                    if (admits[c]) {
                        var t = clone();
                        t.assign.AddLast(c);
                        t = t.search();
                        if (t != null) {
                            return t;
                        }
                    }
                }

                return null;
            }
        }

        override public String ToString() {
            var res = new char[n * n];
            for (var i = 0; i < n; i++) {
                for (var j = 0; j < n; j++) {
                    for (var k = 0; k < n; k++) {
                        if (admits[coord(i, j, k)]) {
                            res[i * n + j] = digits[k];
                        }
                    }
                }
            }
            return new String(res);
        }
    }
}
