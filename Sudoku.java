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
    static final int COORD = SIZE * SIZE * SIZE;
    static final int BLOCK = VIEW * SIZE * SIZE;

    static final int OPEN   = 0;
    static final int FIXED  = 1;
    static final int BANNED = 2;

    static final int DONE = SIZE + 1;

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
                    List<Integer> list = new ArrayList<>(VIEW);
                    list.add(block(GRD, i, j));
                    list.add(block(ROW, i, k));
                    list.add(block(COL, j, k));
                    list.add(block(BOX, p, k));
                    parents.add(coord(i, j, k), list);
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

    int[] state;
    int[] count;
    Queue<Integer> queue = new LinkedList<>();

    Sudoku() {
        state = new int[COORD];
        Arrays.fill(state, OPEN);
        count = new int[BLOCK];
        Arrays.fill(count, SIZE);
    }

    Sudoku(Sudoku that) {
        state = Arrays.copyOf(that.state, COORD);
        count = Arrays.copyOf(that.count, BLOCK);
    }

    Sudoku fix(int c) {
        queue.add(c);
        return this;
    }

    Sudoku read(String input) {
        int len = input.length();
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                if (i * SIZE + j < len) {
                    int k = DIGITS.indexOf(input.charAt(i * SIZE + j));
                    if (k != -1) {
                        fix(coord(i, j, k));
                    }
                }
            }
        }
        return this;
    }

    Sudoku search() throws NoSolutionException {

        while (!queue.isEmpty()) {
            int c0 = queue.remove();
            if (state[c0] == FIXED) {
                continue;
            }
            if (state[c0] == BANNED) {
                throw new NoSolutionException();
            }
            state[c0] = FIXED;
            for (int b1 : parents.get(c0)) {
                count[b1] = DONE;
                for (int c2 : children.get(b1)) {
                    if (c2 != c0 && state[c2] != BANNED) {
                        state[c2] = BANNED;
                        for (int b3 : parents.get(c2)) {
                            if (b3 != b1) {
                                count[b3]--;
                                if (count[b3] == 0) {
                                    throw new NoSolutionException();
                                }
                                if (count[b3] == 1) {
                                    for (int c4 : children.get(b3)) {
                                        if (state[c4] == OPEN) {
                                            fix(c4);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        int min = DONE;
        int bmin = -1;
        for (int b = 0; b < BLOCK; b++) {
            if (count[b] < min) {
                min = count[b];
                bmin = b;
            }
        }

        if (min == DONE) {
            return this;
        }

        for (int c : children.get(bmin)) {
            if (state[c] == OPEN) {
                Sudoku s = new Sudoku(this);
                try {
                    return s.fix(c).search();
                }
                catch (NoSolutionException e) {
                }
            }
        }

        throw new NoSolutionException();
    }

    public String toString() {
        char[] output = new char[SIZE * SIZE];
        Arrays.fill(output, '.');
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                for (int k = 0; k < SIZE; k++) {
                    if (state[coord(i, j, k)] == FIXED) {
                        output[i * SIZE + j] = DIGITS.charAt(k);
                    }
                }
            }
        }
        return new String(output);
    }
}
