import java.io.*;
import java.util.*;

class Sudoku {

    public static void main(String args[]) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        Sudoku s = new Sudoku();
        String input;
        while ((input = br.readLine()) != null) {
            if (s.clear().read(input) && s.solve()) {
                System.out.println(s);
            }
            else {
                System.out.println("No solution");
            }
        }
    }

    private static final int SIZE    = 3;
    private static final int N_DIGIT = SIZE * SIZE;
    private static final int N_COORD = N_DIGIT * N_DIGIT * N_DIGIT;
    private static final int GRD     = 0;
    private static final int ROW     = 1;
    private static final int COL     = 2;
    private static final int BOX     = 3;
    private static final int N_VIEW  = 4;
    private static final int N_BLOCK = N_VIEW * N_DIGIT * N_DIGIT;
    private static final int DEFINED = 0xDEF;

    private final boolean[] admits;
    private final int[]     option;

    public Sudoku() {
        admits = new boolean[N_COORD];
        option = new int[N_BLOCK];
        clear();
    }

    public boolean read(String input) {
        for (int ij = 0; ij < input.length() && ij < N_DIGIT * N_DIGIT; ij++) {
            char c = input.charAt(ij);
            if (c < '1' || c > '9') {
                continue;
            }
            int i = ij / N_DIGIT;
            int j = ij % N_DIGIT;
            int k = (int)c - (int)'1';
            if (!assign(coordOf(i, j, k))) {
                return false;
            }
        }
        return true;
    }

    public boolean solve() {
        int min = DEFINED;
        for (int o : option) {
            if (o < min) {
                min = o;
            }
        }
        if (min == DEFINED) {
            return true;
        }
        int b = 0;
        while (option[b] != min) {
            b++;
        }
        Sudoku saved = new Sudoku().copy(this);
        for (int c : children.get(b)) {
            if (saved.admits[c] && copy(saved).assign(c) && solve()) {
                return true;
            }
        }
        return false;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder(N_DIGIT * N_DIGIT);
        for (int i = 0; i < N_DIGIT; i++) {
            for (int j = 0; j < N_DIGIT; j++) {
                if (option[coordOf(GRD, i, j)] == DEFINED) {
                    int k = 0;
                    while (!admits[coordOf(i, j, k)]) {
                        k++;
                    }
                    sb.append((char)((int)'1' + k));
                }
                else {
                    sb.append('.');
                }
            }
        }
        return sb.toString();
    }

    private Sudoku clear() {
        for (int c = 0; c < N_COORD; c++) {
            admits[c] = true;
        }
        for (int b = 0; b < N_BLOCK; b++) {
            option[b] = N_DIGIT;
        }
        return this;
    }

    private Sudoku copy(Sudoku s) {
        for (int c = 0; c < N_COORD; c++) {
            admits[c] = s.admits[c];
        }
        for (int b = 0; b < N_BLOCK; b++) {
            option[b] = s.option[b];
        }
        return this;
    }

    private boolean assign(int c) {
        if (!admits[c]) {
            return false;
        }
        Set<Integer> bs = new TreeSet<>();
        for (int cc : siblings.get(c)) {
            if (admits[cc]) {
                admits[cc] = false;
                for (int b : parents.get(cc)) {
                    option[b]--;
                    bs.add(b);
                }
            }
        }
        for (int b : parents.get(c)) {
            option[b] = DEFINED;
        }
        for (int b : bs) {
            if (!check(b)) {
                return false;
            }
        }
        return true;
    }

    private boolean check(int b) {
        switch (option[b]) {
            case 0:
                return false;
            case 1:
                int k = 0;
                while (!admits[children.get(b).get(k)]) {
                    k++;
                }
                return assign(children.get(b).get(k));
            default:
                return true;
        }
    }

    private static int coordOf(int i, int j, int k) {
        return (i * N_DIGIT + j) * N_DIGIT + k;
    }

    private static int blockOf(int v, int i, int j) {
        return (v * N_DIGIT + i) * N_DIGIT + j;
    }

    private static final List<List<Integer>> parents;
    private static final List<List<Integer>> children;
    private static final List<List<Integer>> siblings;

    static {
        parents  = new ArrayList<>(N_COORD);
        children = new ArrayList<>(N_BLOCK);
        siblings = new ArrayList<>(N_COORD);
        for (int i = 0; i < N_DIGIT; i++) {
            for (int j = 0; j < N_DIGIT; j++) {
                int p = i / SIZE * SIZE + j / SIZE;
                for (int k = 0; k < N_DIGIT; k++) {
                    parents.add(coordOf(i, j, k), Arrays.asList(
                                blockOf(GRD, i, j),
                                blockOf(ROW, i, k),
                                blockOf(COL, j, k),
                                blockOf(BOX, p, k))
                            );
                }
            }
        }
        for (int b = 0; b < N_BLOCK; b++) {
            children.add(b, new ArrayList<Integer>(N_VIEW));
        }
        for (int c = 0; c < N_COORD; c++) {
            for (int b : parents.get(c)) {
                children.get(b).add(c);
            }
        }
        for (int c = 0; c < N_COORD; c++) {
            Set<Integer> cs = new TreeSet<>();
            for (int b : parents.get(c)) {
                for (int cc : children.get(b)) {
                    if (cc != c) {
                        cs.add(cc);
                    }
                }
            }
            siblings.add(c, new ArrayList<Integer>(cs));
        }
    }
}
