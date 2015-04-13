module Sudoku where

import Data.Array.Unboxed
import Data.List
import Data.Maybe

data Sudoku = Sudoku (Array View (Array (Int, Int) (Maybe Int))) (UArray Int Bool) deriving Eq

data View = GRD | ROW | COL | BOX deriving (Bounded, Enum, Eq, Ord, Ix)

views :: [View]
views = [minBound .. maxBound]

chars :: String
chars = ['1' .. '9']

m :: Int
m = 3

n :: Int
n= m * m

untilN :: [Int]
untilN = [0 .. n - 1]

toSudoku :: String -> Sudoku
toSudoku str = foldl aux newSudoku [ (i, j) | i <- untilN , j <- untilN ] where
    aux sudoku (i, j)
        = case elemIndex (lns !! i !! j) chars of
            Just k
                -> fill sudoku (toCoord GRD (i, j, k))
            _
                -> sudoku
        where lns = lines str

instance Show Sudoku where
    show (Sudoku det _)
        = unlines $ flip map untilN $ \ i ->
            [ toChar (det ! GRD ! (i, j)) | j <- untilN ]
        where
            toChar (Just k) = chars !! k
            toChar Nothing  = '.'

newSudoku :: Sudoku
newSudoku = Sudoku det poss where
    det  = constArray (minBound, maxBound) (constArray ((0, 0), (n - 1, n - 1)) Nothing)
    poss = constArray (0, n * n * n - 1) True
    constArray bds value = array bds [ (i, value) | i <- range bds ]

fill :: Sudoku -> Int -> Sudoku
fill (Sudoku det poss) p
    | poss ! p && and [ isNothing $ det ! v ! (i, j) | v <- views , let (i, j, _) = fromCoord v p ]
        = Sudoku det' poss'
    | otherwise
        = error "fill"
    where
        det'  = det // [ (v, det ! v // [((i, j), Just k)]) | v <- views , let (i, j, k) = fromCoord v p ]
        poss' = poss // [ (toCoord v (i, j, k), False) | v <- views, let (i, j, _) = fromCoord v p , k <- untilN ]

erase :: Sudoku -> Int -> Sudoku
erase (Sudoku det poss) p = Sudoku det poss' where
    poss' = poss // [(p, False)]

toCoord :: View -> (Int, Int, Int) -> Int
toCoord GRD (i, j, k) = i * n * n + j * n + k
toCoord ROW (i, j, k) = i * n * n + k * n + j
toCoord COL (i, j, k) = k * n * n + i * n + j
toCoord BOX (i, j, k) = (i `div` m * m + k `div` m) * n * n + (i `mod` m * m + k `mod` m) * n + j

fromCoord :: View -> Int -> (Int, Int, Int)
fromCoord v p = case v of
    GRD -> (i, j, k)
    ROW -> (i, k, j)
    COL -> (j, k, i)
    BOX -> (i `div` m * m + j `div` m, k, i `mod` m * m + j `mod` m)
    where
        i = p `div` n `div` n
        j = p `div` n `mod` n
        k = p `mod` n

solve :: Sudoku -> [Sudoku]
solve s = case simplify s of
    Just s' @ (Sudoku _ poss)
        | s /= s'
            -> solve s'
        | isComplete s
            -> [s]
        | otherwise
            -> solve (fill s p) ++ solve (erase s p)
        where p = head (filter (poss !) [0 .. ])
    Nothing
        -> []


isComplete :: Sudoku -> Bool
isComplete (Sudoku det _) = and [ isJust (det ! v ! (i, j)) | v <- views , i <- untilN , j <- untilN ]

simplify :: Sudoku -> Maybe Sudoku
simplify s0 = foldl aux (Just s0) [ (v, i, j) | v <- views , i <- untilN , j <- untilN ] where
    aux (Just s @ (Sudoku det poss)) (v, i, j)
        | isJust $ det ! v ! (i, j)
            = Just s
        | null ks
            = Nothing
        | length ks == 1
            = Just $ fill s (toCoord v (i, j, head ks))
        | otherwise
            = Just s
        where ks = [ k | k <- untilN , poss ! toCoord v (i, j, k) ]
    aux Nothing _
        = Nothing

main :: IO ()
main = interact (aux . solve . toSudoku) where
    aux sudokus
        = "found " ++ show (length sudokus) ++ " answers\n"
        ++ concatMap (("\n" ++) . show) sudokus
