import Control.Monad
import Data.Array.Unboxed
import Data.List
import Data.Tuple

main :: IO ()
main = interact $ unlines . map solve . lines

type Sudoku = (UArray Coord Bool, UArray Block Int)
type Index = Int
type Coord = Int
type Block = Int

unit :: Int
unit = 3

size :: Int
size = unit * unit

idxs :: [Index]
idxs = [0 .. size - 1]

idxs2 :: [(Index, Index)]
idxs2 = [ (i, j) | i <- idxs, j <- idxs ]

defined :: Int
defined = size + 1

digits :: String
digits = "123456789"

coordBounds :: (Coord, Coord)
coordBounds = (0, size * size * size - 1)

blockBounds :: (Block, Block)
blockBounds = (0, 4 * size * size - 1)

coord :: Index -> Index -> Index -> Coord
coord i j k = (i * size + j) * size + k

blocks :: Index -> Index -> Index -> [Block]
blocks i j k = [ (0 * size + i) * size + j
               , (1 * size + i) * size + k
               , (2 * size + j) * size + k
               , (3 * size + p) * size + k
               ]
    where p = i `div` unit * unit + j `div` unit

parents :: Array Coord [Block]
parents = array coordBounds $ do
    i <- idxs
    j <- idxs
    k <- idxs
    return (coord i j k, blocks i j k)


children :: Array Block [Coord]
children = accumArray add [] blockBounds $ do
    (c, bs) <- assocs parents
    b <- bs
    return (b, c)
    where add xs y = xs ++ [y]

siblings :: Array Coord [Coord]
siblings = array coordBounds $ do
    (c, bs) <- assocs parents
    return (c, delete c $ nub $ concatMap (children !) bs)

solve :: String -> String
solve input = case maybeSearch $ readSudoku input of
                   []  -> "NO SOLUTION"
                   [s] -> showSudoku s
                   _   -> "MULTIPLE SOLUTIONS"

maybeSearch :: Maybe Sudoku -> [Sudoku]
maybeSearch = maybe [] search

search :: Sudoku -> [Sudoku]
search s @ (_, count)
    | m == defined = [s]
    | otherwise    = concatMap (maybeSearch . assign s) (children ! b)
    where (m, b) = minimum $ map swap $ assocs count

showSudoku :: Sudoku -> String
showSudoku (state, _) = do
    (i, j) <- idxs2
    return $ case [ k | k <- idxs, state ! coord i j k ] of
                  []  -> 'x'
                  [k] -> digits !! k
                  _   -> '?'

readSudoku :: String -> Maybe Sudoku
readSudoku = foldM assign emptySudoku . toCoords

emptySudoku :: Sudoku
emptySudoku = (state, count)
    where state = listArray coordBounds (repeat True)
          count = listArray blockBounds (repeat size)

toCoords :: String -> [Coord]
toCoords input = do
    ((i, j), d) <- zip idxs2 input
    case elemIndex d digits of
         Just k  -> return $ coord i j k
         Nothing -> mzero

assign :: Sudoku -> Coord -> Maybe Sudoku
assign (state, count) c0
    | state ! c0 = foldM check (state', count') b3s
    | otherwise  = Nothing
    where b1s = parents ! c0
          c2s = filter (state !) (siblings ! c0)
          b3s = concatMap (parents !) c2s
          state' = state // zip c2s (repeat False)
          count' = accum (-) count (zip b3s (repeat 1)) // zip b1s (repeat defined)

check :: Sudoku -> Block -> Maybe Sudoku
check s @ (state, count) b = case count ! b of
                                  0 -> Nothing
                                  1 -> assign s c
                                  _ -> Just s
    where [c] = filter (state !) (children ! b)
