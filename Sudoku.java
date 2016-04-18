import java.io.*;
import java.util.*;

class Sudoku {

    public static void main(String args[]) throws IOException {

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        while (true) {

            String input = br.readLine();
            if (input == null) {
                return;
            }

            try {
                System.out.println(new Sudoku().read(input).solve());
            }
            catch (NoSolutionException e) {
                System.out.println("NO SOLUTION!!!");
            }
        }
    }

    private static class NoSolutionException extends Exception {}

    private static final int M = 3;
    private static final int N = M * M;
    private static final int V = 4;

    private static final String DIGITS = "123456789";

    private static int coordOf(int i, int j, int k) {
        return (i * N + j) * N + k;
    }

    private static int blockOf(int v, int p, int q) {
        return (v * N + p) * N + q;
    }

    private static final List<List<Integer>> parents;
    private static final List<List<Integer>> children;
    private static final List<List<Integer>> siblings;

    static {

        parents  = new ArrayList<>(N * N * N);
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                int p = i / M * M + j / M;
                for (int k = 0; k < N; k++) {
                    List<Integer> bs = new ArrayList<>(V);
                    bs.add(0, blockOf(0, i, j));
                    bs.add(1, blockOf(1, i, k));
                    bs.add(2, blockOf(2, j, k));
                    bs.add(3, blockOf(3, p, k));
                    parents.add(coordOf(i, j, k), bs);
                }
            }
        }

        children = new ArrayList<>(V * N * N);
        for (int b = 0; b < V * N * N; b++) {
            children.add(b, new ArrayList<Integer>(N));
        }
        for (int c = 0; c < N * N * N; c++) {
            for (int b : parents.get(c)) {
                children.get(b).add(c);
            }
        }

        siblings = new ArrayList<>(N * N * N);
        for (int c = 0; c < N * N * N; c++) {
            Set<Integer> ccs = new TreeSet<>();
            for (int b : parents.get(c)) {
                for (int cc : children.get(b)) {
                    ccs.add(cc);
                }
            }
            ccs.remove(c);
            siblings.add(c, new ArrayList<Integer>(ccs));
        }
    }

    private final boolean[] admits;

    private final int[] option;

    public Sudoku() {

        admits = new boolean[N * N * N];
        for (int c = 0; c < admits.length; c++) {
            admits[c] = true;
        }

        option = new int[V * N * N];
        for (int b = 0; b < option.length; b++) {
            option[b] = N;
        }
    }

    public Sudoku(Sudoku s) {

        admits = new boolean[N * N * N];
        for (int c = 0; c < admits.length; c++) {
            admits[c] = s.admits[c];
        }

        option = new int[V * N * N];
        for (int b = 0; b < option.length; b++) {
            option[b] = s.option[b];
        }
    }

    public Sudoku read(String input) throws NoSolutionException {
        for (int i = 0; i < N && i * N < input.length(); i++) {
            for (int j = 0; j < N && i * N + j < input.length(); j++) {
                int k = DIGITS.indexOf(input.charAt(i * N + j));
                if (k != -1) {
                    assign(coordOf(i, j, k));
                }
            }
        }
        return this;
    }

    public Sudoku solve() throws NoSolutionException {

        int b = -1;
        int min = N + 1;
        for (int bb = 0; bb < option.length; bb++) {
            if (option[bb] > 1 && option[bb] < min) {
                b = bb;
                min = option[b];
            }
        }

        if (b == -1) {
            return this;
        }

        for (int c : children.get(b)) {
            if (admits[c]) {
                try {
                    return new Sudoku(this).assign(c).solve();
                }
                catch (NoSolutionException e) {
                }
            }
        }

        throw new NoSolutionException();
    }

    public String toString() {
        char[] result = new char[N * N];
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                switch (option[blockOf(0, i, j)]) {
                    case 0:
                        result[i * N + j] = 'x';
                        break;
                    case 1:
                        for (int k = 0; k < N; k++) {
                            if (admits[coordOf(i, j, k)]) {
                                result[i * N + j] = DIGITS.charAt(k);
                            }
                        }
                        break;
                    default:
                        result[i * N + j] = '?';
                }
            }
        }
        return new String(result);
    }

    private Sudoku assign(int c) throws NoSolutionException {
        if (!admits[c]) {
            throw new NoSolutionException();
        }
        for (int cc : siblings.get(c)) {
            forbid(cc);
        }
        return this;
    }

    private Sudoku forbid(int c) throws NoSolutionException {
        if (admits[c]) {
            admits[c] = false;
            for (int b : parents.get(c)) {
                reduce(b);
            }
        }
        return this;
    }

    private Sudoku reduce(int b) throws NoSolutionException {
        option[b]--;
        if (option[b] == 0) {
            throw new NoSolutionException();
        }
        if (option[b] == 1) {
            for (int c : children.get(b)) {
                if (admits[c]) {
                    assign(c);
                }
            }
        }
        return this;
    }
}
