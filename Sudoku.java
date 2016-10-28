import java.io.*;
import java.util.*;

public class Sudoku {

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String input;
        while ((input = in.readLine()) != null) {
            System.out.println(solve(input));
        }
    }

    static String solve(String input) {
        Sudoku s = new Sudoku();
        try {
            return s.read(input).search().toString();
        }
        catch (NoSolutionException e) {
            return "NO SOLUTION";
        }
    }

    private class NoSolutionException extends Exception {}

    static final int GRD   = 0;
    static final int ROW   = 1;
    static final int COL   = 2;
    static final int BOX   = 3;
    static final int VIEW  = 4;
    static final int UNIT  = 3;
    static final int SIZE  = UNIT * UNIT;
    static final int BLOCK = VIEW * SIZE * SIZE;
    static final int COORD = SIZE * SIZE * SIZE;

    static final int DEFINED    = 0xDEF;
    static final int POSSIBLE   = 0;
    static final int IMPOSSIBLE = 1;
    static final int FIXED      = 2;

    static final String DIGITS = "123456789";

    static int coord(int i, int j, int k) {
        return (i * SIZE + j) * SIZE + k;
    }

    static int block(int v, int p, int q) {
        return (v * SIZE + p) * SIZE + q;
    }

    static List<List<Integer>> parents  = new ArrayList<>(COORD);
    static List<List<Integer>> children = new ArrayList<>(BLOCK);

    static {
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                int p = i / UNIT * UNIT + j / UNIT;
                for (int k = 0; k < SIZE; k++) {
                    List<Integer> bs = new ArrayList<>(VIEW);
                    bs.add(block(GRD, i, j));
                    bs.add(block(ROW, i, k));
                    bs.add(block(COL, j, k));
                    bs.add(block(BOX, p, k));
                    parents.add(coord(i, j, k), bs);
                }
            }
        }
        for (int b = 0; b < BLOCK; b++) {
            children.add(b, new ArrayList<>(SIZE));
        }
        for (int c = 0; c < COORD; c++) {
            for (int b : parents.get(c)) {
                children.get(b).add(c);
            }
        }
    }

    int[] bstate;
    int[] cstate;
    Queue<Integer> queue = new LinkedList<>();

    Sudoku() {
        bstate = new int[BLOCK];
        Arrays.fill(bstate, SIZE);
        cstate = new int[COORD];
        Arrays.fill(cstate, POSSIBLE);
    }

    Sudoku(Sudoku that) {
        bstate = Arrays.copyOf(that.bstate, BLOCK);
        cstate = Arrays.copyOf(that.cstate, COORD);
    }

    Sudoku search() throws NoSolutionException {

        while (!queue.isEmpty()) {
            int c0 = queue.remove();
            if (cstate[c0] == FIXED) {
                continue;
            }
            if (cstate[c0] == IMPOSSIBLE) {
                throw new NoSolutionException();
            }
            cstate[c0] = FIXED;
            for (int b1 : parents.get(c0)) {
                bstate[b1] = DEFINED;
                for (int c2 : children.get(b1)) {
                    if (c2 != c0 && cstate[c2] != IMPOSSIBLE) {
                        cstate[c2] = IMPOSSIBLE;
                        for (int b3 : parents.get(c2)) {
                            if (b3 != b1) {
                                bstate[b3]--;
                                if (bstate[b3] == 0) {
                                    throw new NoSolutionException();
                                }
                                if (bstate[b3] == 1) {
                                    for (int c4 : children.get(b3)) {
                                        if (cstate[c4] == POSSIBLE) {
                                            assign(c4);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        int min = DEFINED;
        int bmin = -1;
        for (int b = 0; b < BLOCK; b++) {
            if (bstate[b] < min) {
                bmin = b;
                min = bstate[bmin];
            }
        }

        if (min == DEFINED) {
            return this;
        }

        for (int c : children.get(bmin)) {
            if (cstate[c] == POSSIBLE) {
                Sudoku s = new Sudoku(this);
                try {
                    return s.assign(c).search();
                }
                catch (NoSolutionException e) {
                }
            }
        }

        throw new NoSolutionException();
    }

    Sudoku assign(int c) {
        queue.add(c);
        return this;
    }

    Sudoku read(String input) {
        for (int ij = 0; ij < SIZE * SIZE; ij++) {
            if (ij >= input.length()) {
                break;
            }
            int k = DIGITS.indexOf(input.charAt(ij));
            if (k != -1) {
                assign(coord(ij / SIZE, ij % SIZE, k));
            }
        }
        return this;
    }

    public String toString() {
        char[] output = new char[SIZE * SIZE];
        Arrays.fill(output, '.');
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                for (int k = 0; k < SIZE; k++) {
                    if (cstate[coord(i, j, k)] == FIXED) {
                        output[i * SIZE + j] = DIGITS.charAt(k);
                    }
                }
            }
        }
        return new String(output);
    }
}
