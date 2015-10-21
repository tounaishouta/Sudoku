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
    grids = [ combine [i] [j] digits | i <- digits, j <- digits ]
    rows  = [ combine [i] digits [k] | i <- digits, k <- digits ]
    cols  = [ combine digits [j] [k] | j <- digits, k <- digits ]
    boxes = [ combine is js [k] | is <- blocks, js <- blocks, k <- digits ]
    combine is js ks = [ coord ! (i, j, k) | i <- is, j <- js, k <- ks ]

parents :: Array Coord [Unit]
parents = accumArray add [] boundsCoord cus where
    add us u = us ++ [u]
    cus = [ (c, u) | (u, cs) <- assocs children, c <- cs ]

siblings :: Array Coord [Coord]
siblings = array boundsCoord (map aux $ assocs parents) where
    aux (c, us) = (c, delete c $ nub $ concatMap (children !) us)

defined :: Int
defined = maxBound

solve :: MonadPlus m => String -> m Sudoku
solve string = solveRec =<< foldM assign emptySudoku cs where
    ijs = [ (i, j) | i <- digits, j <- digits ]
    ks  = concat $ words string
    cs  = [ coord ! (i, j, k) | ((i, j), k) <- zip ijs ks, k `elem` digits ]

solveRec :: MonadPlus m => Sudoku -> m Sudoku
solveRec s @ (a, o)
    | n == defined
        = return s
    | otherwise
        = msum [ solveRec =<< assign s c | c <- children ! u, a ! c ]
    where (n, u) = minimum $ map swap $ assocs o

assign :: MonadPlus m => Sudoku -> Coord -> m Sudoku
assign (a, o) c
    | a ! c
        = foldM check (a', o') us
    | otherwise
        = mzero
    where
        a' = a // zip' cs False
        o' = accum (-) o (zip' us 1) // zip' (parents ! c) defined
        cs = filter (a !) (siblings ! c)
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
showSudoku (a, _) =  [ toChar i j | j <- digits, i <- digits ] where
    toChar i j = case [ k | k <- digits, a ! (coord ! (i, j, k)) ] of
        [k] -> k
        _   -> '.'
