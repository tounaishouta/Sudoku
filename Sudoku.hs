module Sudoku where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Array.Unboxed  (Array, UArray, array, range, (!), (//))
import Data.Ix             (Ix)
import Data.Maybe          (isJust)
import Data.List           (elemIndex, intercalate)

main :: IO ()
main = interact (intercalate "\n" . map show . solve . read)

data Sudoku = Sudoku
    { definite :: Array (V, I, I) (Maybe I)
    , possible :: UArray (I, I, I) Bool
    } deriving (Eq)

data V = GRD | ROW | COL | BOX deriving (Bounded, Ix, Eq, Ord)

m :: Int
m = 3

n :: Int
n = m * m

newtype I = I { unI :: Int } deriving (Ix, Eq, Ord)

instance Bounded I where
    minBound = I 0
    maxBound = I (n - 1)

fromGRD :: (I, I, I) -> [(V, I, I, I)]
fromGRD (i, j, k) =
    [ (GRD, i, j, k)
    , (ROW, k, i, j)
    , (COL, k, j, i)
    , (BOX, k, p, q)
    ] where
        p = I (unI i `div` m * m + unI j `div` m)
        q = I (unI i `mod` m * m + unI j `mod` m)

toGRD :: (V, I, I, I) -> (I, I, I)
toGRD (GRD, i, j, k) = (i, j, k)
toGRD (ROW, k, i, j) = (i, j, k)
toGRD (COL, k, j, i) = (i, j, k)
toGRD (BOX, k, p, q) = (i, j, k) where
    i = I (unI p `div` m * m + unI q `div` m)
    j = I (unI p `mod` m * m + unI q `mod` m)

whole :: (Bounded a, Ix a) => [a]
whole = range (minBound, maxBound)

empty :: Sudoku
empty = Sudoku { definite = d , possible = p } where
    d = constArray Nothing
    p = constArray True
    constArray y = array (minBound, maxBound) [ (x, y) | x <- whole ]

isCompleted :: Sudoku -> Bool
isCompleted s = and [ isJust (definite s ! vij) | vij <- whole ]

fill :: Sudoku -> (I, I, I) -> Sudoku
fill s g = s { definite = d , possible = p } where
    d = definite s // [ ((v, i, j), Just k) | (v, i, j, k) <- fromGRD g ]
    p = possible s // [ (toGRD (v, i, j, k), False) | (v, i, j, _) <- fromGRD g , k <- whole ]

erase :: Sudoku -> (I, I, I) -> Sudoku
erase s g = s { possible = p } where
    p = possible s // [(g, False)]

reduce :: Sudoku -> Maybe Sudoku
reduce s0 = foldM aux s0 (whole :: [(V, I, I)]) where
    aux s vij @ (v, i, j)
        | isJust (definite s ! vij)
            = Just s
        | otherwise
            = case [ k | k <- whole , possible s ! toGRD (v, i, j, k) ] of
                []  -> Nothing
                [k] -> Just (fill s (toGRD (v, i, j, k)))
                _   -> Just s

solve :: Sudoku -> [Sudoku]
solve s
    | isCompleted s
        = [s]
    | otherwise
        = case reduce s of
            Just s'
                | s == s'
                    -> solve (fill s g0) ++ solve (erase s g0)
                | otherwise
                    -> solve s'
                where g0 = head [ g | g <- whole , possible s ! g ]
            Nothing
                -> []

chars :: String -- [Char]
chars = ['1' .. '9']

instance Read Sudoku where
    readsPrec _ input = [(s0, unlines (drop n lns))] where
        s0 = foldl aux empty (whole :: [(I, I)])
        aux s (i, j) = case I <$> elemIndex (lns !! unI i !! unI j) chars of
            Just k  -> fill s (i, j, k)
            Nothing -> s
        lns = lines input

instance Show Sudoku where
    show s = unlines [ [ toChar (definite s ! (GRD, i, j)) | j <- whole ] | i <- whole ] where
        toChar (Just k) = chars !! unI k
        toChar Nothing  = '.'
