import java.io.*;
import java.util.*;

public class Sudoku {

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            String s = br.readLine();
            if (s == null) {
                break;
            }
            try {
                System.out.println(new Sudoku(s).solve().toString());
            }
            catch (NoSolutionException nse) {
                System.out.println("no solution");
            }
        }
    }

    static class NoSolutionException extends Exception {}

    static final int size    = 3;
    static final int nDigit  = size * size;
    static final int nCoord  = nDigit * nDigit * nDigit;
    static final int GRD     = 0;
    static final int ROW     = 1;
    static final int COL     = 2;
    static final int BOX     = 3;
    static final int nView   = 4;
    static final int nBlock  = nView * nDigit * nDigit;
    static final int defined = 0xDEF;

    static int toCoord(int i, int j, int k) {
        return (i * nDigit + j) * nDigit + k;
    }

    static int toBlock(int v, int p, int q) {
        return (v * nDigit + p) * nDigit + q;
    }

    static ArrayList<ArrayList<Integer>> parents;
    static ArrayList<ArrayList<Integer>> children;
    static ArrayList<ArrayList<Integer>> siblings;

    static {
        parents = new ArrayList<>(nCoord);
        for (int i = 0; i < nDigit; i++) {
            for (int j = 0; j < nDigit; j++) {
                int p = i / size * size + j / size;
                for (int k = 0; k < nDigit; k++) {
                    int c = toCoord(i, j, k);
                    parents.add(c, new ArrayList<Integer>(nView));
                    parents.get(c).add(toBlock(GRD, i, j));
                    parents.get(c).add(toBlock(ROW, i, k));
                    parents.get(c).add(toBlock(COL, j, k));
                    parents.get(c).add(toBlock(BOX, p, k));
                }
            }
        }
        children = new ArrayList<>(nBlock);
        for (int b = 0; b < nBlock; b++) {
            children.add(b, new ArrayList<Integer>(nDigit));
        }
        for (int c = 0; c < nCoord; c++) {
            for (int b : parents.get(c)) {
                children.get(b).add(c);
            }
        }
        siblings = new ArrayList<>(nCoord);
        for (int c = 0; c < nCoord; c++) {
            siblings.add(c, new ArrayList<Integer>(nView * nDigit));
            for (int b : parents.get(c)) {
                for (int c1 : children.get(b)) {
                    if (c1 != c) {
                        siblings.get(c).add(c1);
                    }
                }
            }
        }
    }

    private BitSet             admits;
    private ArrayList<Integer> choice;

    private Sudoku() {
        admits = new BitSet(nCoord);
        for (int c = 0; c < nCoord; c++) {
            admits.set(c);
        }
        choice = new ArrayList<Integer>(nBlock);
        for (int b = 0; b < nBlock; b++) {
            choice.add(b, nDigit);
        }
    }

    private Sudoku(Sudoku s) {
        admits = new BitSet(nCoord);
        for (int c = 0; c < nCoord; c++) {
            admits.set(c, s.admits.get(c));
        }
        choice = new ArrayList<Integer>(nBlock);
        for (int b = 0; b < nBlock; b++) {
            choice.add(b, s.choice.get(b));
        }
    }

    public Sudoku(String input) throws NoSolutionException {
        this();
        for (int i = 0; i < nDigit; i++) {
            for (int j = 0; j < nDigit; j++) {
                int k = (int)(input.charAt(i * nDigit + j) - '1');
                if (0 <= k && k < nDigit) {
                    assign(toCoord(i, j, k));
                }
            }
        }
    }

    public Sudoku solve() throws NoSolutionException {
        int b0 = -1;
        int min = defined;
        for (int b = 0; b < nBlock; b++) {
            if (choice.get(b) < min) {
                min = choice.get(b);
                b0 = b;
            }
        }
        if (min == defined) {
            return this;
        }
        for (int c : children.get(b0)) {
            if (admits.get(c)) {
                try {
                    return new Sudoku(this).assign(c).solve();
                }
                catch (NoSolutionException nse) {
                    continue;
                }
            }
        }
        throw new NoSolutionException();
    }

    private Sudoku assign(int c0) throws NoSolutionException {
        if (! admits.get(c0)) {
            throw new NoSolutionException();
        }
        ArrayList<Integer> bs = new ArrayList<>(nView * nDigit);
        for (int c : siblings.get(c0)) {
            if (admits.get(c)) {
                admits.clear(c);
                for (int b : parents.get(c)) {
                    choice.set(b, choice.get(b) - 1);
                    bs.add(b);
                }
            }
        }
        for (int b : parents.get(c0)) {
            choice.set(b, defined);
        }
        for (int b : bs) {
            check(b);
        }
        return this;
    }

    private Sudoku check(int b) throws NoSolutionException {
        switch (choice.get(b)) {
            case 0:
                throw new NoSolutionException();
            case 1:
                for (int c : children.get(b)) {
                    if (admits.get(c)) {
                        return assign(c);
                    }
                }
            default:
                return this;
        }
    }

    public String toString() {
        StringBuffer sb = new StringBuffer(nDigit * nDigit);
        for (int i = 0; i < nDigit; i++) {
            for (int j = 0; j < nDigit; j++) {
                if (choice.get(toBlock(GRD, i, j)) == defined) {
                    int k = 0;
                    while (! admits.get(toCoord(i, j, k))) {
                        k++;
                    }
                    sb.append((char)('1' + k));
                }
                else {
                    sb.append('.');
                }
            }
        }
        return sb.toString();
    }
}
