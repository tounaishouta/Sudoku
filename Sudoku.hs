module Main where

import Data.Array.Unboxed (Array, UArray, array, range, (!), (//))
import Data.Ix            (Ix)
import Data.List          (elemIndex, foldl', intercalate)

main :: IO ()
main = interact (intercalate "\n" . map show . solve . read)

data V = GRD | ROW | COL | BOX deriving (Bounded, Enum, Eq, Ix, Ord)

m :: Int
m = 3

n :: Int
n = m * m

type I = Int

iMin :: I
iMin = 0

iMax :: I
iMax = n - 1

is :: [I]
is = [iMin .. iMax]

type C = (I, I, I)

cMin :: C
cMin = (iMin, iMin, iMin)

cMax :: C
cMax = (iMax, iMax, iMax)

toC :: (V, I, I, I) -> C
toC (GRD, i, j, k) = (i, j, k)
toC (ROW, i, j, k) = (j, k, i)
toC (COL, i, j, k) = (k, j, i)

fromC :: C -> [(V, I, I, I)]
fromC (i, j, k) = 
    [ (GRD, i, j, k)
    , (ROW, k, i, j)
    , (COL, k, j, i)
    ]

chars :: String -- [Char]
chars = ['1' .. '9']

data Sudoku = Sudoku
    { definite :: Array (V, I, I) (Maybe I)
    , possible :: UArray C Bool
    } deriving Eq

instance Read Sudoku where
    readsPrec _ input = [(s0, unlines (drop n lns))] where
        s0 = foldl' aux empty [ (i, j) | i <- is , j <- is ]
        aux s (i, j) = case elemIndex (lns !! i !! j) chars of
            Just k  -> fill s (toC (GRD, i, j, k))
            Nothing -> s
        lns = lines input

instance Show Sudoku where
    show s = unlines [ [ toChar (definite s ! (GRD, i, j)) | j <- is ] | i <- is ] where
        toChar (Just k) = chars !! k
        toChar Nothing  = '.'

empty :: Sudoku
empty = Sudoku { definite = d , possible = p } where
    d = constArray ((minBound, iMin, iMin), (maxBound, iMax, iMax)) Nothing
    p = constArray (cMin, cMax) True
    constArray bds val = array bds [ (i, val) | i <- range bds ]

fill :: Sudoku -> C -> Sudoku
fill s c
    | possible s ! c
        = s { definite = d , possible = p }
    | otherwise
        = error "cannot fill here"
    where
        d = definite s // [ ((v, i, j), Just k) | (v, i, j, k) <- fromC c ]
        p = possible s // [ (toC (v, i, j, k), False) | (v, i, j, _) <- fromC c , k <- is ]

solve :: Sudoku -> [Sudoku]
solve s = [s]
