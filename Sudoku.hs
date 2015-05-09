module Main where

import Control.Monad (foldM)
import Data.Array    (Array, accum, array, (!))
import Data.Ix       (Ix, range)
import Data.List     (delete, intercalate)

main :: IO ()
main = interact $ intercalate "\n" . map showBoard . solve . readBoard

m :: Int
m = 3

n :: Int
n = m * m

newtype I = I { unI :: Int } deriving (Eq, Ix, Ord)

instance Bounded I where
    minBound = I 0
    maxBound = I (n - 1)

data V = GRD | ROW | COL | BOX deriving (Bounded, Eq, Ix, Ord)

whole :: (Bounded a, Ix a) => [a]
whole = range (minBound, maxBound)

data State = Definite I | Indefinite [I]

type Board = Array (V, I, I) State

solve :: Board -> [Board]
solve b
    | isComplete b
        = [b]
    | otherwise
        = case reduce b of
            Nothing -> []
            Just b'
                | filled b < filled b'
                    -> solve b'
                | otherwise
                    -> concat [ solve $ set b (vij, k) | k <- possibles b vij ]
                where vij = head $ filter (not . isDefinite . (b !)) whole

isComplete :: Board -> Bool
isComplete b = filled b == n * n

filled :: Board -> Int
filled b = length [ (i, j) | (i, j) <- whole , isDefinite $ b ! (GRD, i, j) ]

isDefinite :: State -> Bool
isDefinite (Definite   _) = True
isDefinite (Indefinite _) = False

reduce :: Board -> Maybe Board
reduce b = foldM check b whole

check :: Board -> (V, I, I) -> Maybe Board
check b vij = case b ! vij of
    Indefinite []  -> Nothing
    Indefinite [k] -> Just $ set b (vij, k)
    _              -> Just b

set :: Board -> ((V, I, I), I) -> Board
set b vijk = accum determine (accum remove b diff) (convert vijk) where
    diff = concat [ convert (vij, k') | (vij, k) <- convert vijk , k' <- possibles b vij , k /= k' ]

possibles :: Board -> (V, I, I) -> [I]
possibles b vij = case b ! vij of
    Definite    _ -> error "possibles: already definite"
    Indefinite ks -> ks

convert :: ((V, I, I), I) -> [((V, I, I), I)]
convert ((GRD, i, j), k) =
    [ ((GRD, i, j), k)
    , ((ROW, k, i), j)
    , ((COL, k, j), i)
    , ((BOX, k, p), q)
    ] where
        p = I $ unI i `div` m * m + unI j `div` m
        q = I $ unI i `mod` m * m + unI j `mod` m

convert ((ROW, k, i), j) = convert ((GRD, i, j), k)
convert ((COL, k, j), i) = convert ((GRD, i, j), k)
convert ((BOX, k, p), q) = convert ((GRD, i, j), k) where
    i = I $ unI p `div` m * m + unI q `div` m
    j = I $ unI p `mod` m * m + unI q `mod` m

determine :: State -> I -> State
determine (Indefinite ks) k
    | k `elem` ks
        = Definite k
    | otherwise
        = error "determine: impossible choice"
determine (Definite _) _
    = error "determine: already definite"

remove :: State -> I -> State
remove (Indefinite ks) k = Indefinite $ delete k ks
remove (Definite    _) _ = error "remove: already definite"

showBoard :: Board -> String
showBoard b = unlines [ [ aux i j | j <- whole ] | i <- whole ] where
    aux i j = case b ! (GRD, i, j) of
        Definite   k -> showI k
        Indefinite _ -> '.'

readBoard :: String -> Board
readBoard s = foldl aux empty whole where
    aux b (i, j) = case readI $ lines s !! unI i !! unI j of
        Nothing -> b
        Just k  -> set b ((GRD, i, j), k)
    empty = array (minBound, maxBound) [ (vij, Indefinite whole) | vij <- whole ]

showI :: I -> Char
showI = (['1' .. '9'] !!) . unI

readI :: Char -> Maybe I
readI c = lookup c [ (showI i, i) | i <- whole ]
