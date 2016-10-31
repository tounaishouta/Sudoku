import Data.Array.Unboxed
import Data.List

main :: IO ()
main = interact $ unlines . map (showSudoku . head . solutions . readSudoku) . lines

type Sudoku = UArray (Int, Int) Char

solutions :: Sudoku -> [Sudoku]
solutions s = case [ p | p <- range bds, s ! p == '.' ] of
                []  -> [s]
                p:_ -> concatMap solutions [ s // [(p, d)] | d <- ds ]
                    where ds = "123456789" \\ [ s ! q | q <- range bds, p ~~ q ]

(~~) :: (Int, Int) -> (Int, Int) -> Bool
(x, y) ~~ (z, w) = x == z || y == w || (x`div`3 == z`div`3 && y`div`3 == w`div`3)

bds :: ((Int, Int), (Int, Int))
bds = ((0, 0), (8, 8))

readSudoku :: String -> Sudoku
readSudoku = listArray bds

showSudoku :: Sudoku -> String
showSudoku = elems
