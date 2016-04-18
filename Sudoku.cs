using System;
using System.Collections.Generic;
using System.Linq;

namespace Sudoku {

    public class Sudoku {

        public static void Main() {
            while (true) {
                var input = Console.ReadLine();
                if (input == null) {
                    return;
                }
                try {
                    Console.WriteLine(Solve(input));
                }
                catch (NoSolutionException) {
                    Console.WriteLine("NO SOLUTION!!!");
                }
            }
        }

        public static String Solve(String input) {
            return new Sudoku().read(input).search().toString();
        }

        class NoSolutionException : Exception {}

        const int m = 3;
        const int n = m * m;
        const int v = 4;

        const String digits = "123456789";

        static int coord(int i, int j, int k) {
            return (i * n + j) * n + k;
        }

        static int block(int p, int q, int r) {
            return (p * n + q) * n + r;
        }

        static List<int>[] parents;
        static List<int>[] children;
        static List<int>[] siblings;

        static Sudoku() {

            parents = new List<int>[n * n * n];
            for (var i = 0; i < n; i++) {
                for (var j = 0; j < n; j++) {
                    var q = i / m * m + j / m;
                    for (var k = 0; k < n; k++) {
                        var bs = new List<int>(v);
                        bs.Add(block(0, i, j));
                        bs.Add(block(1, i, k));
                        bs.Add(block(2, j, k));
                        bs.Add(block(3, q, k));
                        parents[coord(i, j, k)] = bs;
                    }
                }
            }

            children = new List<int>[v * n * n];
            for (var b = 0; b < children.Length; b++) {
                children[b] = new List<int>(n);
            }
            for (var c = 0; c < parents.Length; c++) {
                foreach (var b in parents[c]) {
                    children[b].Add(c);
                }
            }

            siblings = new List<int>[n * n * n];
            for (var c = 0; c < siblings.Length; c++) {
                var ccs = new HashSet<int>();
                foreach (var b in parents[c]) {
                    foreach (var cc in children[b]) {
                        ccs.Add(cc);
                    }
                }
                ccs.Remove(c);
                siblings[c] = ccs.ToList();
            }
        }

        bool[] admits;

        int[] rest;

        Sudoku() {

            admits = new bool[n * n * n];
            for (var c = 0; c < admits.Length; c++) {
                admits[c] = true;
            }

            rest = new int[v * n * n];
            for (var b = 0; b < rest.Length; b++) {
                rest[b] = n;
            }
        }

        Sudoku(Sudoku s) {

            admits = new bool[n * n * n];
            s.admits.CopyTo(admits, 0);

            rest = new int[v * n * n];
            s.rest.CopyTo(rest, 0);
        }

        Sudoku read(String input) {
            for (var i = 0; i < n; i++) {
                for (var j = 0; j < n; j++) {
                    if (i * n + j < input.Length) {
                        var k = digits.IndexOf(input[i * n + j]);
                        if (k != -1) {
                            assign(coord(i, j, k));
                        }
                    }
                }
            }
            return this;
        }

        Sudoku search() {

            var b = -1;
            var min = n + 1;
            for (var bb = 0; bb < rest.Length; bb++) {
                if (rest[bb] > 1 && rest[bb] < min) {
                    b = bb;
                    min = rest[b];
                }
            }

            if (b == -1) {
                return this;
            }

            foreach (var c in children[b]) {
                if (admits[c]) {
                    var s = new Sudoku(this);
                    try {
                        return s.assign(c).search();
                    }
                    catch (NoSolutionException) {
                    }
                }
            }

            throw new NoSolutionException();
        }

        Sudoku assign(int c) {
            if (!admits[c]) {
                throw new NoSolutionException();
            }
            foreach (var cc in siblings[c]) {
                forbid(cc);
            }
            return this;
        }

        Sudoku forbid(int c) {
            if (admits[c]) {
                admits[c] = false;
                foreach (var b in parents[c]) {
                    reduce(b);
                }
            }
            return this;
        }

        Sudoku reduce(int b) {
            rest[b]--;
            if (rest[b] == 0) {
                throw new NoSolutionException();
            }
            if (rest[b] == 1) {
                foreach (var c in children[b]) {
                    if (admits[c]) {
                        assign(c);
                    }
                }
            }
            return this;
        }

        String toString() {
            var output = new char[n * n];
            for (var i = 0; i < n; i++) {
                for (var j = 0; j < n; j++) {
                    for (var k = 0; k < n; k++) {
                        if (admits[coord(i, j, k)]) {
                            output[i * n + j] = digits[k];
                        }
                    }
                }
            }
            return new String(output);
        }
    }
}
