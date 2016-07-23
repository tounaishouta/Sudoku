import Control.Monad (foldM)
import Data.Array.Unboxed
import Data.List (delete, elemIndex, nub)
import Data.Tuple (swap)

main :: IO ()
main = interact $ unlines . map solve . lines

type Sudoku = (UArray Coord Bool, UArray Block Int)
type Coord  = Int
type Block  = Int
data View = GRD | ROW | COL | BOX

view :: Int
view = 4

unit :: Int
unit = 3

size :: Int
size = unit * unit

idxs :: [Int]
idxs = [0 .. size - 1]

digits :: String
digits = "123456789"

coordBounds :: (Coord, Coord)
coordBounds = (0, size * size * size - 1)

blockBounds :: (Block, Block)
blockBounds = (0, view * size * size - 1)

coord :: Int -> Int -> Int -> Coord
coord i j k = (i * size + j) * size + k

block :: View -> Int -> Int -> Block
block GRD i j = (0 * size + i) * size + j
block ROW i k = (1 * size + i) * size + k
block COL j k = (2 * size + j) * size + k
block BOX p k = (3 * size + p) * size + k

parents :: Array Coord [Block]
parents = array coordBounds (do
    i <- idxs
    j <- idxs
    let p = i `div` unit * unit + j `div` unit
    k <- idxs
    let bs = [block GRD i j, block ROW i k, block COL j k, block BOX p k]
    [(coord i j k, bs)])

children :: Array Block [Coord]
children = accumArray add [] coordBounds (do
    (c, bs) <- assocs parents
    b <- bs
    [(b, c)])
        where add xs y = xs ++ [y]

siblings :: Array Coord [Coord]
siblings = array coordBounds (do
    (c, bs) <- assocs parents
    let cs = delete c $ nub $ concatMap (children !) bs
    [(c, cs)])

defined :: Int
defined = 0xDEF

solve :: String -> String
solve input = case maybe [] search (readSudoku input) of
                []    -> "NO SOLUTION"
                s : _ -> showSudoku s

search :: Sudoku -> [Sudoku]
search s @ (_, count)
  | m == defined = [s]
  | otherwise    = concatMap (maybe [] search . assign s) (children ! b)
  where (m, b) = minimum $ map swap $ assocs count

assign :: Sudoku -> Coord -> Maybe Sudoku
assign (admit, count) c
  | admit ! c = foldM check (admit', count') bs
  | otherwise = Nothing
  where cs = filter (admit !) (siblings ! c)
        bs = concatMap (parents !) cs
        admit' = admit // zip cs (repeat False)
        count' = accum (-) count (zip bs (repeat 1)) // zip (parents ! c) (repeat defined)

check :: Sudoku -> Block -> Maybe Sudoku
check s @ (admit, count) b = case count ! b of
                               0 -> Nothing
                               1 -> assign s c
                               _ -> Just s
                               where [c] = filter (admit !) (children ! b)

readSudoku :: String -> Maybe Sudoku
readSudoku input = foldM assign emptySudoku (do
    i <- idxs
    j <- idxs
    let ij = i * size + j
    if ij < length input
       then case elemIndex (input !! ij) digits of
              Nothing -> []
              Just k  -> [coord i j k]
        else [])

emptySudoku :: Sudoku
emptySudoku = (constArray coordBounds True, constArray blockBounds size) where
    constArray bds val = array bds [ (i, val) | i <- range bds ]

showSudoku :: Sudoku -> String
showSudoku (admit, _) = do
    i <- idxs
    j <- idxs
    let ks = filter ((admit !) . coord i j) idxs
    case ks of
      [k] -> [digits !! k]
      _   -> ['.']
