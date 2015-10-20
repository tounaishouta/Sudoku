import Control.Monad
import Data.Array.Unboxed
import Data.List
import Data.Tuple

main :: IO ()
main = interact $ unlines . map aux . lines where
    aux string = unlines $ string : map showSudokuOneLine (solve string)

type Sudoku = (UArray Coord Bool, UArray Unit Int)

type Digit = Char

digits :: [Digit]
digits = concat blocks

blocks :: [[Digit]]
blocks = ["123", "456", "789"]

type Coord = Int

boundsCoord :: (Coord, Coord)
boundsCoord = bounds fromCoord

toCoord :: UArray (Digit, Digit, Digit) Coord
toCoord = array ((a, a, a), (b, b, b)) $ map swap $ assocs fromCoord where
    a = minimum digits
    b = maximum digits

fromCoord :: Array Coord (Digit, Digit, Digit)
fromCoord = listArray (1, length digit3) digit3 where
    digit3 = [ (i, j, k) | i <- digits, j <- digits, k <- digits ]

type Unit = Int

boundsUnit :: (Unit, Unit)
boundsUnit = bounds children

children :: Array Unit [Coord]
children = listArray (1, length units) units where
    units = grids ++ rows ++ cols ++ boxes
    grids = [ zip' [i] [j] digits | i <- digits, j <- digits ]
    rows  = [ zip' [i] digits [k] | i <- digits, k <- digits ]
    cols  = [ zip' digits [j] [k] | j <- digits, k <- digits ]
    boxes = [ zip' is js [k] | is <- blocks, js <- blocks, k <- digits ]
    zip' is js ks = [ toCoord ! (i, j, k) | i <- is, j <- js, k <- ks ]

parents :: Array Coord [Unit]
parents = accumArray add [] boundsCoord [ (c, u) | (u, cs) <- assocs children, c <- cs ] where
    add us u = us ++ [u]

siblings :: Array Coord [Coord]
siblings = array boundsCoord [ (c, delete c $ nub $ concatMap (children !) us) | (c, us) <- assocs parents ]

defined :: Int
defined = 999

solve :: MonadPlus m => String -> m Sudoku
solve string = solveRec =<< foldM assign emptySudoku cs where
    ijs = [ (i, j) | i <- digits, j <- digits ]
    ks  = concat $ words string
    cs  = [ toCoord ! (i, j, k) | ((i, j), k) <- zip ijs ks, k `elem` digits ]

solveRec :: MonadPlus m => Sudoku -> m Sudoku
solveRec s @ (a, o)
    | all (== defined) $ elems o
        = return s
    | otherwise
        = msum [ solveRec =<< assign s c | c <- children ! u, a ! c ]
    where
        (_, u) = minimum $ map swap $ assocs o

assign :: MonadPlus m => Sudoku -> Coord -> m Sudoku
assign (a, o) c
    | a ! c
        = foldM check (a', o') us
    | otherwise
        = mzero
    where
        a' = a // zip' cs False
        o' = accum (-) o (zip' us 1) // zip' (parents ! c) defined
        cs = filter (a !) $ siblings ! c
        us = concatMap (parents !) cs
        zip' xs y = zip xs (repeat y)

check :: MonadPlus m => Sudoku -> Unit -> m Sudoku
check s @ (a, o) u = case o ! u of
    0 -> mzero
    1 -> assign s c
    _ -> return s
    where [c] = filter (a !) (children ! u)

emptySudoku :: Sudoku
emptySudoku = (a, o) where
    a = accumArray const True boundsCoord []
    o = accumArray const (length digits) boundsUnit []

showSudoku :: Sudoku -> String
showSudoku (a, _) = unlines [ [ aux i j | j <- digits ] | i <- digits ] where
    aux i j = case [ k | k <- digits, a ! (toCoord ! (i, j, k)) ] of
        [k] -> k
        _   -> '.'

showSudokuOneLine :: Sudoku -> String
showSudokuOneLine = concat . lines . showSudoku
