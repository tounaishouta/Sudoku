import Control.Monad      (foldM)
import Data.Array.Unboxed (Array, Ix, UArray, accum, accumArray, array, assocs, (!), (//))
import Data.Char          (chr, ord)
import Data.List          (delete, nub)
import Data.Tuple         (swap)

main :: IO ()
main = interact $ unlines . map (showSudoku . head . solveMaybe . readSudoku) . lines

newtype Sudoku = Sudoku (UArray Coord Bool, UArray Block Int)

solve :: Sudoku -> [Sudoku]
solve s @ (Sudoku (_, o))
    | m == defined = [s]
    | otherwise    = concatMap (solveMaybe . assign s) (children ! b)
    where (m, b) = minimum $ map swap (assocs o)

solveMaybe :: Maybe Sudoku -> [Sudoku]
solveMaybe = maybe [] solve

assign :: Sudoku -> Coord -> Maybe Sudoku
assign (Sudoku (a, o)) c
    | a ! c     = foldM check (Sudoku (a', o')) bs
    | otherwise = Nothing
    where
        a' = a // zip cs (repeat False)
        o' = accum (-) o (zip bs (repeat 1)) // zip (parents ! c) (repeat defined)
        cs = filter (a !) (siblings ! c)
        bs = concatMap (parents !) cs

check :: Sudoku -> Block -> Maybe Sudoku
check s @ (Sudoku (a, o)) b = case o ! b of
    0 -> Nothing
    1 -> assign s c
    _ -> return s
    where [c] = filter (a !) (children ! b)

showSudoku :: Sudoku -> String
showSudoku (Sudoku (a, _)) = [ show' i j | i <- digits, j <- digits ] where
    show' i j = case filter ((a !) . toCoord i j) digits of
        [k] -> fromDigit k
        _   -> '.'
    fromDigit k = chr $ ord '1' + k

readSudoku :: String -> Maybe Sudoku
readSudoku str = foldM assign emptySudoku cs where
    cs = [ toCoord i j (toDigit ch) | ((i, j), ch) <- zip ijs str, isDigit ch ]
    ijs = [ (i, j) | i <- digits, j <- digits ]
    isDigit ch = '1' <= ch && ch <= '9'
    toDigit ch = ord ch - ord '1'

emptySudoku :: Sudoku
emptySudoku = Sudoku (accumArray const True boundsCoord [], accumArray const nDigit boundsBlock [])

size :: Int
size = 3

nDigit :: Int
nDigit = size * size

digits :: [Int]
digits = take nDigit [0 .. ]

newtype Coord  = Coord Int deriving (Eq, Ord, Ix)

toCoord :: Int -> Int -> Int -> Coord
toCoord i j k = Coord $ (i * nDigit + j) * nDigit + k

boundsCoord :: (Coord, Coord)
boundsCoord = (toCoord 0 0 0, toCoord (nDigit - 1) (nDigit - 1) (nDigit - 1)) where

data View = GRD | ROW | COL | BOX

newtype Block  = Block Int deriving (Eq, Ord, Ix)

toBlock :: View -> Int -> Int -> Block
toBlock GRD i j = Block $ (0 * nDigit + i) * nDigit + j
toBlock ROW i j = Block $ (1 * nDigit + i) * nDigit + j
toBlock COL i j = Block $ (2 * nDigit + i) * nDigit + j
toBlock BOX i j = Block $ (3 * nDigit + i) * nDigit + j

boundsBlock :: (Block, Block)
boundsBlock = (toBlock GRD 0 0, toBlock BOX (nDigit - 1) (nDigit - 1)) where

defined :: Int
defined = 0xDEF

parents :: Array Coord [Block]
parents = array boundsCoord
    [ (toCoord i j k, [toBlock GRD i j, toBlock ROW i k, toBlock COL j k, toBlock BOX p k])
    | i <- digits , j <- digits , let p = i `div` size * size + j `div` size , k <- digits ]

children :: Array Block [Coord]
children = accumArray (flip (:)) [] boundsBlock [ (b, c) | (c, bs) <- assocs parents, b <- bs ]

siblings :: Array Coord [Coord]
siblings = array boundsCoord [ (c, delete c . nub $ concatMap (children !) bs) | (c, bs) <- assocs parents ]
