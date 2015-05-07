import Control.Monad      (foldM)
import Data.Array.Unboxed (Array, UArray, array, elems, (!), (//))
import Data.Ix            (Ix, range)
import Data.Maybe         (isJust)
import Data.List          (elemIndex, intercalate)

main :: IO ()
main = interact (intercalate "\n" . map showBoard . solve . readBoard)

data Board = Board
    { definite :: Array (V, I, I) (Maybe I)
    , possible :: UArray (I, I, I) Bool
    } deriving (Eq)

m :: Int
m = 3

n :: Int
n = m * m

newtype I = I { unI :: Int } deriving (Ix, Eq, Ord)

instance Bounded I where
    minBound = I 0
    maxBound = I (n - 1)

data V = GRD | ROW | COL | BOX deriving (Bounded, Ix, Eq, Ord)

whole :: (Bounded a, Ix a) => [a]
whole = range (minBound, maxBound)

fromGRD :: (I, I, I) -> [(V, I, I, I)]
fromGRD (i, j, k) =
    [ (GRD, i, j, k)
    , (ROW, k, i, j)
    , (COL, k, j, i)
    , (BOX, k, p, q)
    ] where
        p = I $ unI i `div` m * m + unI j `div` m
        q = I $ unI i `mod` m * m + unI j `mod` m

toGRD :: (V, I, I, I) -> (I, I, I)
toGRD (GRD, i, j, k) = (i, j, k)
toGRD (ROW, k, i, j) = (i, j, k)
toGRD (COL, k, j, i) = (i, j, k)
toGRD (BOX, k, p, q) = (i, j, k) where
    i = I $ unI p `div` m * m + unI q `div` m
    j = I $ unI p `mod` m * m + unI q `mod` m

empty :: Board
empty = Board { definite = constArray Nothing , possible = constArray True } where
    constArray y = array (minBound, maxBound) [ (x, y) | x <- whole ]

isCompleted :: Board -> Bool
isCompleted = all isJust . elems . definite

fill :: Board -> (I, I, I) -> Board
fill s c
    | possible s ! c
        = s { definite = d , possible = p }
    | otherwise
        = error "can not fill here"
    where
        d = definite s // [ ((v, i, j), Just k) | (v, i, j, k) <- fromGRD c ]
        p = possible s // [ (toGRD (v, i, j, k), False) | (v, i, j, _) <- fromGRD c , k <- whole ]

erase :: Board -> (I, I, I) -> Board
erase s c = s { possible = p } where
    p = possible s // [(c, False)]

reduce :: Board -> Maybe Board
reduce s = foldM check s whole

check :: Board -> (V, I, I) -> Maybe Board
check s vij @ (v, i, j)
    | isJust (definite s ! vij)
        = Just s
    | otherwise
        = case filter (possible s !) [ toGRD (v, i, j, k) | k <- whole ] of
            []  -> Nothing
            [c] -> Just $ fill s c
            _   -> Just s

solve :: Board -> [Board]
solve s
    | isCompleted s
        = [s]
    | otherwise
        = case reduce s of
            Just s'
                | s == s'
                    -> solve (fill s c) ++ solve (erase s c)
                | otherwise
                    -> solve s'
                where c = head $ filter (possible s !) whole
            Nothing
                -> []

chars :: [Char]
chars = ['1' .. '9']

readBoard :: String -> Board
readBoard input = foldl aux empty whole where
    aux s (i, j) = case elemIndex (lines input !! unI i !! unI j) chars of
        Just _k -> fill s (i, j, I _k)
        Nothing -> s

showBoard :: Board -> String
showBoard s = unlines [ [ toChar (definite s ! (GRD, i, j)) | j <- whole ] | i <- whole ] where
    toChar (Just k) = chars !! unI k
    toChar Nothing  = '.'
