import java.io.*;
import java.util.*;

class Sudoku {

    public static void main(String[] args) {
        try {
            BufferedReader in =
                new BufferedReader(new InputStreamReader(System.in));
            String input;
            while ((input = in.readLine()) != null) {
                System.out.println(solve(input));
            }
        }
        catch (Exception e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
    }

    static String solve(String input) {
        try {
            return new Sudoku().read(input).search().show();
        }
        catch (NoSolutionException e) {
            return "NO SOLUTION";
        }
    }

    static class NoSolutionException extends Exception {
        private static final long serialVersionUID = 0;
    }

    static enum View { GRD, ROW, COL, BOX }
    static enum State { OPEN, FIXED, BANNED }

    static final int VIEW  = 4;
    static final int UNIT  = 3;
    static final int SIZE  = UNIT * UNIT;
    static final int COORD = SIZE * SIZE * SIZE;
    static final int BLOCK = VIEW * SIZE * SIZE;
    static final int DONE  = SIZE + 1;

    static final String DIGITS = "123456789";

    static List<List<Integer>> parents;
    static List<List<Integer>> children;

    static int coord(int i, int j, int k) {
        return (i * SIZE + j) * SIZE + k;
    }

    static int block(View v, int p, int q) {
        return (v.ordinal() * SIZE + p) * SIZE + q;
    }

    static {
        parents = new ArrayList<>(COORD);
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                int p = i / UNIT * UNIT + j / UNIT;
                for (int k = 0; k < SIZE; k++) {
                    parents.add(coord(i, j, k), Arrays.asList(
                                block(View.GRD, i, j),
                                block(View.ROW, i, k),
                                block(View.COL, j, k),
                                block(View.BOX, p, k)));
                }
            }
        }
        children = new ArrayList<>(BLOCK);
        for (int b = 0; b < BLOCK; b++) {
            children.add(b, new ArrayList<>(SIZE));
        }
        for (int c = 0; c < COORD; c++) {
            for (int b : parents.get(c)) {
                children.get(b).add(c);
            }
        }
    }

    State[]        state = new State[COORD];
    int[]          count = new int[BLOCK];
    Queue<Integer> queue = new LinkedList<>();

    Sudoku() {
        Arrays.fill(state, State.OPEN);
        Arrays.fill(count, SIZE);
    }

    Sudoku(Sudoku that) {
        System.arraycopy(that.state, 0, state, 0, COORD);
        System.arraycopy(that.count, 0, count, 0, BLOCK);
    }

    Sudoku enqueue(int c) {
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
                        enqueue(coord(i, j, k));
                    }
                }
            }
        }
        return this;
    }

    String show() {
        char[] result = new char[SIZE * SIZE];
        Arrays.fill(result, '.');
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                for (int k = 0; k < SIZE; k++) {
                    if (state[coord(i, j, k)] == State.FIXED) {
                        result[i * SIZE + j] = DIGITS.charAt(k);
                    }
                }
            }
        }
        return new String(result);
    }

    Sudoku search() throws NoSolutionException {

        while (!queue.isEmpty()) {
            int c = queue.remove();
            switch (state[c]) {
                case OPEN:
                    fix(c);
                    break;
                case FIXED:
                    break;
                case BANNED:
                    throw new NoSolutionException();
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
            if (state[c] == State.OPEN) {
                try {
                    return new Sudoku(this).enqueue(c).search();
                }
                catch (NoSolutionException e) {
                }
            }
        }

        throw new NoSolutionException();
    }

    void fix(int c) throws NoSolutionException {
        state[c] = State.FIXED;
        for (int b : parents.get(c)) {
            mark(b);
        }
    }

    void mark(int b) throws NoSolutionException {
        count[b] = DONE;
        for (int c : children.get(b)) {
            if (state[c] == State.OPEN) {
                ban(c);
            }
        }
    }

    void ban(int c) throws NoSolutionException {
        state[c] = State.BANNED;
        for (int b : parents.get(c)) {
            if (count[b] != DONE) {
                countdown(b);
            }
        }
    }

    void countdown(int b) throws NoSolutionException {
        count[b]--;
        if (count[b] == 0) {
            throw new NoSolutionException();
        }
        if (count[b] == 1) {
            for (int c : children.get(b)) {
                if (state[c] == State.OPEN) {
                    enqueue(c);
                }
            }
        }
    }
}
