import java.io.*;
import java.util.*;

class Sudoku {

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String input;
        while ((input = in.readLine()) != null) {
            System.out.println(solve(input));
        }
    }

    public static String solve(String input) {
        try {
            return new Sudoku().read(input).search().toString();
        }
        catch (NoSolutionException e) {
            return "NO SOLUTION";
        }
    }

    private class NoSolutionException extends Exception {}

    private static final int UNIT  = 3;
    private static final int SIZE  = UNIT * UNIT;
    private static final int GRD   = 0;
    private static final int ROW   = 1;
    private static final int COL   = 2;
    private static final int BOX   = 3;
    private static final int VIEW  = 4;
    private static final int COORD = SIZE * SIZE * SIZE;
    private static final int BLOCK = VIEW * SIZE * SIZE;

    private static final int DEFINED = 0xDEF;

    private static final String DIGITS = "123456789";

    private static int coord(int i, int j, int k) {
        return (i * SIZE + j) * SIZE + k;
    }

    private static int block(int v, int p, int q) {
        return (v * SIZE + p) * SIZE + q;
    }

    private static List<List<Integer>> parents  = new ArrayList<List<Integer>>(COORD);
    private static List<List<Integer>> children = new ArrayList<List<Integer>>(BLOCK);

    static {
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                int p = i / UNIT * UNIT + j / UNIT;
                for (int k = 0; k < SIZE; k++) {
                    List<Integer> bs = new ArrayList<Integer>(VIEW);
                    bs.add(block(GRD, i, j));
                    bs.add(block(ROW, i, k));
                    bs.add(block(COL, j, k));
                    bs.add(block(BOX, p, k));
                    parents.add(coord(i, j, k), bs);
                }
            }
        }
        for (int b = 0; b < BLOCK; b++) {
            children.add(b, new ArrayList<Integer>(SIZE));
        }
        for (int c = 0; c < COORD; c++) {
            for (int b : parents.get(c)) {
                children.get(b).add(c);
            }
        }
    }

    private final boolean[] admit = new boolean[COORD];

    private final int[] count = new int[BLOCK];

    private Sudoku() {
        for (int c = 0; c < COORD; c++) {
            admit[c] = true;
        }
        for (int b = 0; b < BLOCK; b++) {
            count[b] = SIZE;
        }
    }

    private Sudoku(Sudoku that) {
        for (int c = 0; c < COORD; c++) {
            admit[c] = that.admit[c];
        }
        for (int b = 0; b < BLOCK; b++) {
            count[b] = that.count[b];
        }
    }

    private Sudoku search() throws NoSolutionException {

        int min = SIZE + 1;
        int bmin = -1;
        for (int b = 0; b < BLOCK; b++) {
            if (count[b] < min) {
                min = count[b];
                bmin = b;
            }
        }
        if (bmin == -1) {
            return this;
        }

        for (int c : children.get(bmin)) if (admit[c]) {
            try {
                return new Sudoku(this).assign(c).search();
            }
            catch (NoSolutionException e) {
            }
        }

        throw new NoSolutionException();
    }

    private Sudoku read(String input) throws NoSolutionException {
        for (int ij = 0; ij < Math.min(input.length(), SIZE * SIZE); ij++) {
            int k = DIGITS.indexOf(input.charAt(ij));
            if (k != -1) {
                assign(coord(ij / SIZE, ij % SIZE, k));
            }
        }
        return this;
    }

    private Sudoku assign(int c) throws NoSolutionException {
        Queue<Integer> queue = new LinkedList<Integer>();
        queue.add(c);
        while (!queue.isEmpty()) {
            int c0 = queue.poll();
            if (!admit[c0]) {
                throw new NoSolutionException();
            }
            for (int b1 : parents.get(c0)) {
                count[b1] = DEFINED;
                for (int c2 : children.get(b1)) if (c2 != c0 && admit[c2]) {
                    admit[c2] = false;
                    for (int b3 : parents.get(c2)) if (b3 != b1) {
                        count[b3]--;
                        if (count[b3] == 0) {
                            throw new NoSolutionException();
                        }
                        if (count[b3] == 1) {
                            for (int c4 : children.get(b3)) if (admit[c4]) {
                                queue.add(c4);
                            }
                        }
                    }
                }
            }
        }
        return this;
    }

    public String toString() {
        char[] output = new char[SIZE * SIZE];
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                int ij = i * SIZE + j;
                List<Integer> ks = new ArrayList<Integer>(SIZE);
                for (int k = 0; k < SIZE; k++) if (admit[coord(i, j, k)]) {
                    ks.add(k);
                }
                if (ks.size() == 1) {
                    output[ij] = DIGITS.charAt(ks.get(0));
                }
                else {
                    output[ij] = '.';
                }
            }
        }
        return new String(output);
    }
}
