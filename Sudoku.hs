import Control.Monad      (foldM)
import Data.Array.Unboxed (Array, UArray, array, elems, (!), (//))
import Data.Ix            (Ix, range)
import Data.List          (intercalate)
import Data.Maybe         (isJust)

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
    , possible :: UArray (I, I, I) Bool
    } deriving (Eq)

solve :: Board -> [Board]
solve s
    | isComplete s
        = [s]
    | otherwise
        = case reduce s of
            Just s'
                | s == s'
                    -> solve (fill s ijk) ++ solve (erase s ijk)
                | otherwise
                    -> solve s'
                where ijk = head $ filter (possible s !) whole
            Nothing
                -> []

isComplete :: Board -> Bool
isComplete = all isJust . elems . definite

reduce :: Board -> Maybe Board
reduce s = foldM check s whole

check :: Board -> (V, I, I) -> Maybe Board
check s vij
    | isJust $ definite s ! vij
        = Just s
    | otherwise
        = case filter (possible s !) [ toI3 vij k | k <- whole ] of
            []    -> Nothing
            [ijk] -> Just $ fill s ijk
            _     -> Just s

fill :: Board -> (I, I, I) -> Board
fill s ijk
    | possible s ! ijk
        = s { definite = d , possible = p }
    | otherwise
        = error "can not fill here"
    where
        d = definite s // [ (vij, Just k) | (vij, k) <- fromI3 ijk ]
        p = possible s // [ (toI3 vij k, False) | (vij, _) <- fromI3 ijk , k <- whole ]

erase :: Board -> (I, I, I) -> Board
erase s ijk = s { possible = possible s // [(ijk, False)] }

fromI3 :: (I, I, I) -> [((V, I, I), I)]
fromI3 (i, j, k) = [((GRD, i, j), k), ((ROW, k, i), j), ((COL, k, j), i), ((BOX, k, p), q)] where
        p = I $ unI i `div` m * m + unI j `div` m
        q = I $ unI i `mod` m * m + unI j `mod` m

toI3 :: (V, I, I) -> I -> (I, I, I)
toI3 (GRD, i, j) k = (i, j, k)
toI3 (ROW, k, i) j = (i, j, k)
toI3 (COL, k, j) i = (i, j, k)
toI3 (BOX, k, p) q = (i, j, k) where
    i = I $ unI p `div` m * m + unI q `div` m
    j = I $ unI p `mod` m * m + unI q `mod` m

showBoard :: Board -> String
showBoard s = unlines [ [ maybe '.' showI $ definite s ! (GRD, i, j) | j <- whole ] | i <- whole ]

readBoard :: String -> Board
readBoard str = foldl aux empty whole where
    aux s (i, j) = case readI $ lines str !! unI i !! unI j of
        Just k  -> fill s (i, j, k)
        Nothing -> s
    empty = Board { definite = constArray Nothing , possible = constArray True }
    constArray y = array (minBound, maxBound) [ (x, y) | x <- whole ]

showI :: I -> Char
showI = (['1' .. '9'] !!) . unI

readI :: Char -> Maybe I
readI c = lookup c [ (showI i, i) | i <- whole ]
