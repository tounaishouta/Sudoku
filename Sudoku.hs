import Control.Monad
import Data.Array.Unboxed
import Data.List
import Data.Tuple

main :: IO ()
main = interact $ unlines . map (maybe "no solution" showSudoku . solve) . lines

type Sudoku = (UArray Coord Bool, UArray Unit Int)

type Digit = Char

digits :: [Digit]
digits = concat blocks

blocks :: [[Digit]]
blocks = ["123", "456", "789"]

type Coord = Int

boundsCoord :: (Coord, Coord)
boundsCoord = (minimum coords, maximum coords) where
    coords = elems coord

coord :: Array (Digit, Digit, Digit) Coord
coord = array ((a, a, a), (b, b, b)) (zip ijks [1 .. ]) where
    ijks = [ (i, j, k) | i <- digits, j <- digits, k <- digits ]
    a = minimum digits
    b = maximum digits

type Unit = Int

boundsUnit :: (Unit, Unit)
boundsUnit = bounds children

children :: Array Unit [Coord]
children = listArray (1, length units) units where
    units = grids ++ rows ++ cols ++ boxes
    grids = [ [ coord ! (i, j, k) | k <- digits ] | i <- digits, j <- digits ]
    rows  = [ [ coord ! (i, j, k) | j <- digits ] | i <- digits, k <- digits ]
    cols  = [ [ coord ! (i, j, k) | i <- digits ] | j <- digits, k <- digits ]
    boxes = [ [ coord ! (i, j, k) | i <- is, j <- js ] | is <- blocks, js <- blocks, k <- digits ]

parents :: Array Coord [Unit]
parents = accumArray (flip (:)) [] boundsCoord [ (c, u) | (u, cs) <- assocs children, c <- cs ]

siblings :: Array Coord [Coord]
siblings = array boundsCoord [ (c, delete c $ nub $ concatMap (children !) us) | (c, us) <- assocs parents ]

determined :: Int
determined = maxBound

solve :: MonadPlus m => String -> m Sudoku
solve ks = solveRec =<< foldM assign emptySudoku cs where
    cs  = [ coord ! (i, j, k) | ((i, j), k) <- zip ijs ks, k `elem` digits ]
    ijs = [ (i, j) | i <- digits, j <- digits ]

solveRec :: MonadPlus m => Sudoku -> m Sudoku
solveRec s @ (a, o)
    | n == determined
        = return s
    | otherwise
        = msum [ solveRec =<< assign s c | c <- children ! u, a ! c ]
    where (n, u) = minimum $ map swap $ assocs o

assign :: MonadPlus m => Sudoku -> Coord -> m Sudoku
assign (a, o) c
    | not $ a ! c
        = mzero
    | otherwise
        = foldM check (a', o') us where
            a' = a // [ (c', False) | c' <- cs ]
            o' = accum (-) o [ (u, 1) | u <- us ] // [ (u, determined) | u <- parents ! c ]
            cs = filter (a !) (siblings ! c)
            us = concatMap (parents !) cs

check :: MonadPlus m => Sudoku -> Unit -> m Sudoku
check s @ (a, o) u = case o ! u of
    0 -> mzero
    1 -> assign s c
    _ -> return s
    where [c] = filter (a !) (children ! u)

emptySudoku :: Sudoku
emptySudoku = (accumArray const True boundsCoord [], accumArray const (length digits) boundsUnit [])

showSudoku :: Sudoku -> String
showSudoku (a, _) =  [ showGrid i j | i <- digits, j <- digits ] where
    showGrid i j = case [ k | k <- digits, a ! (coord ! (i, j, k)) ] of
        [k] -> k
        _ -> '.'
