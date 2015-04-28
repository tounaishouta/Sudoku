module Main where

import Control.Monad      (foldM)
import Data.Array.Unboxed (Array, UArray, array, range, (!), (//))
import Data.Ix            (Ix)
import Data.List          (elemIndex, intercalate)
import Data.Maybe         (isJust)

main :: IO ()
main = interact (intercalate "\n" . map show . solve . read)

data V = GRD | ROW | COL | BOX deriving (Bounded, Enum, Eq, Ix, Ord)

vs :: [V]
vs = [minBound .. maxBound]

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

cs :: [C]
cs = [ (i, j, k) | i <- is , j <- is , k <- is ]

toC :: (V, I, I, I) -> C
toC (GRD, i, j, k) = (i, j, k)
toC (ROW, k, i, j) = (i, j, k)
toC (COL, k, j, i) = (i, j, k)
toC (BOX, k, p, q) = (i, j, k) where
    i = p `div` m * m + q `div` m
    j = p `mod` m * m + q `mod` m

fromC :: C -> [(V, I, I, I)]
fromC (i, j, k) =
    [ (GRD, i, j, k)
    , (ROW, k, i, j)
    , (COL, k, j, i)
    , (BOX, k, p, q)
    ] where
        p = i `div` m * m + j `div` m
        q = i `mod` m * m + j `mod` m

chars :: [Char]
chars = ['1' .. '9']

data Sudoku = Sudoku
    { definite :: Array (V, I, I) (Maybe I)
    , possible :: UArray C Bool
    } deriving Eq

instance Read Sudoku where
    readsPrec _ input = [(s0, unlines (drop n lns))] where
        s0 = foldl aux empty [ (i, j) | i <- is , j <- is ]
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
    constArray bds val = array bds [ (x, val) | x <- range bds ]

isComplete :: Sudoku -> Bool
isComplete s = and [ isJust (definite s ! (v, i, j)) | v <- vs , i <- is , j <- is ]

fill :: Sudoku -> C -> Sudoku
fill s c
    | possible s ! c
        = s { definite = d , possible = p }
    | otherwise
        = error "cannot fill here"
    where
        d = definite s // [ ((v, i, j), Just k) | (v, i, j, k) <- fromC c ]
        p = possible s // [ (toC (v, i, j, k), False) | (v, i, j, _) <- fromC c , k <- is ]

erase :: Sudoku -> C -> Sudoku
erase s c = s { possible = possible s // [(c, False)] }

reduce :: Sudoku -> Maybe Sudoku
reduce s0 = foldM aux s0 [ (v, i, j) | v <- vs , i <- is , j <- is ] where
    aux s vij @ (v, i, j)
        | isJust (definite s ! vij)
            = Just s
        | otherwise
            = case [ k | k <- is , possible s ! toC (v, i, j, k) ] of
                []  -> Nothing
                [k] -> Just (fill s (toC (v, i, j, k)))
                _   -> Just s

solve :: Sudoku -> [Sudoku]
solve s
    | isComplete s
        = [s]
    | otherwise
        = case reduce s of
            Just s'
                | s == s'
                    -> solve (fill s c) ++ solve (erase s c)
                | otherwise
                    -> solve s'
                where c = head (filter (possible s !) cs)
            Nothing
                -> []
