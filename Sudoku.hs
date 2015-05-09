import Control.Monad (foldM)
import Data.Array    (Array, accum, array, (!), (//))
import Data.Ix       (Ix, range)
import Data.List     (intercalate, delete)

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

data Board = Board
    { definite :: Array (V, I, I) (Maybe I)
    , possible :: Array (V, I, I) [I]
    , progress :: Int
    }

solve :: Board -> [Board]
solve b
    | isComplete b
        = [b]
    | otherwise
        = case reduce b of
            Nothing -> []
            Just b'
                | progress b < progress b'
                    -> solve b'
                | otherwise
                    -> solve (set b vijk) ++ solve (unset b vijk)
                where vijk = head [ (vij, k) | vij <- whole , k <- possible b ! vij ]

isComplete :: Board -> Bool
isComplete b = progress b == n * n

reduce :: Board -> Maybe Board
reduce b = foldM check b whole

check :: Board -> (V, I, I) -> Maybe Board
check b vij = case (definite b ! vij, possible b ! vij) of
    (Nothing, [])  -> Nothing
    (Nothing, [k]) -> Just $ set b (vij, k)
    _              -> Just b

set :: Board -> ((V, I, I), I) -> Board
set b vijk = b
    { definite = definite b // [ (vij, Just k) | (vij, k) <- convert vijk ]
    , possible = accum (flip delete) (possible b) diff
    , progress = progress b + 1
    } where
        diff = concat [ convert (vij, k) | (vij, _) <- convert vijk , k <- possible b ! vij ]

unset :: Board -> ((V, I, I), I) -> Board
unset b vijk = b { possible = accum (flip delete) (possible b) (convert vijk) }

convert :: ((V, I, I) , I) -> [((V, I, I), I)]
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

showBoard :: Board -> String
showBoard b = unlines [ [ maybe '.' showI $ definite b ! (GRD, i, j) | j <- whole ] | i <- whole ]

readBoard :: String -> Board
readBoard s = foldl aux empty whole where
    aux b (i, j) = case readI $ lines s !! unI i !! unI j of
        Nothing -> b
        Just k  -> set b ((GRD, i, j), k)
    empty = Board { definite = constArray Nothing , possible = constArray whole , progress = 0 }
    constArray y = array (minBound, maxBound) [ (x, y) | x <- whole ]

showI :: I -> Char
showI = (['1' .. '9'] !!) . unI

readI :: Char -> Maybe I
readI c = lookup c [ (showI i, i) | i <- whole ]
