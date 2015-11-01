import Data.Array.Unboxed
import Data.List

main :: IO ()
main = interact $ unlines . map showSudoku . solve . readSudoku

type Sudoku = UArray (Int, Int) Char

solve :: Sudoku -> [Sudoku]
solve s = case [ p | (p, d) <- assocs s, d == '.' ] of
    [] -> [s]
    p0 @ (i0, j0) : _ -> concatMap solve [ s // [(p0, d)] | d <- ds ] where
        ds = "123456789" \\ map (s !) ps
        ps = [ (i0, j) | j <- [0 .. 8] ] ++ [ (i, j0) | i <- [0 .. 8] ]
            ++ [ (i0 `div` 3 * 3 + i, j0 `div` 3 * 3 + j) | i <- [0 .. 2], j <- [0 .. 2] ]

readSudoku :: String -> Sudoku
readSudoku = listArray ((0, 0), (8, 8))

showSudoku :: Sudoku -> String
showSudoku = elems
