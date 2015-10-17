import Control.Monad
import Data.Array.Unboxed
import Data.List (delete, nub)
import Data.Tuple (swap)

main :: IO ()
main = interact $ unlines . map showSudoku . solve

type Sudoku = (UArray Coord Bool, UArray Unit Int)

determined :: Int
determined = 999

type Digit = Char

digits :: [Digit]
digits = concat blocks

blocks :: [[Digit]]
blocks = ["123", "456", "789"]

type Coord = (Digit, Digit, Digit)

boundsCoord :: (Coord, Coord)
boundsCoord = ((dMin, dMin, dMin), (dMax, dMax, dMax)) where
    dMin = minimum digits
    dMax = maximum digits

type Unit = Int

boundsUnit :: (Unit, Unit)
boundsUnit = bounds children

children :: Array Unit [Coord]
children = listArray (1, length units) units where
    units = grids ++ rows ++ cols ++ boxes
    grids = [ triples [i] [j] digits | i <- digits , j <- digits ]
    rows  = [ triples [i] digits [k] | i <- digits , k <- digits ]
    cols  = [ triples digits [j] [k] | j <- digits , k <- digits ]
    boxes = [ triples is js [k] | is <- blocks , js <- blocks , k <- digits ]
    triples is js ks = [ (i, j, k) | i <- is , j <- js , k <- ks ]

parents :: Array Coord [Unit]
parents = accumArray (flip (:)) [] boundsCoord [ (c, u) | (u, cs) <- assocs children , c <- cs ]

siblings :: Array Coord [Coord]
siblings = array boundsCoord [ (c, delete c $ nub $ concatMap (children !) us) | (c, us) <- assocs parents ]

solve :: MonadPlus m => String -> m Sudoku
solve string = solveRec =<< foldM fill emptySudoku cs where
    cs  = [ (i, j, k) | ((i, j), k) <- zip ijs ks , k `elem` digits ]
    ijs = [ (i, j) | i <- digits , j <- digits ]
    ks  = concat $ words string

solveRec :: MonadPlus m => Sudoku -> m Sudoku
solveRec s @ (_, o)
    | all (== determined) $ elems o
        = return s
    | otherwise
        = msum [ solveRec =<< fill s c | c <- children ! u ]
    where
        (_, u) = minimum $ map swap $ assocs o

fill :: MonadPlus m => Sudoku -> Coord -> m Sudoku
fill (a, o) c
    | a ! c
        = foldM check (a', o') us
    | otherwise
        = mzero
    where
        a' = a // zip' cs False
        o' = accum (-) o (zip' us 1) // zip' (parents ! c) determined
        cs = filter (a !) $ siblings ! c
        us = concatMap (parents !) cs
        zip' xs y = zip xs (repeat y)

check :: MonadPlus m => Sudoku -> Unit -> m Sudoku
check s @ (a, o) u = case o ! u of
    0 -> mzero
    1 -> fill s c
    _ -> return s
    where
        c = head $ filter (a !) $ children ! u

emptySudoku :: Sudoku
emptySudoku = (a, o) where
    a = accumArray const True boundsCoord []
    o = accumArray const (length digits) boundsUnit []

showSudoku :: Sudoku -> String
showSudoku (a, _) = unlines [ [ aux i j | j <- digits ] | i <- digits ] where
    aux i j = case [ k | k <- digits , a ! (i, j, k) ] of
        [k] -> k
        _   -> '.'
