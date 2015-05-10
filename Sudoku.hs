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

type Board = (Array (V, I, I) (Either [I] I), Int)

state :: Board -> (V, I, I) -> Either [I] I
state = (!) . fst

undetermined :: Board -> Int
undetermined = snd

empty :: Board
empty = (array (minBound, maxBound) [ (vij, Left whole) | vij <- whole], n * n)

solve :: Board -> [Board]
solve b
    | undetermined b == 0
        = [b]
    | otherwise
        = case reduce b of
            Nothing -> []
            Just b'
                | undetermined b > undetermined b'
                    -> solve b'
                | otherwise
                    -> concat [ solve $ set b (vij, k) | k <- fromLeft $ state b vij ]
                where vij = head $ filter (isLeft . state b) whole

reduce :: Board -> Maybe Board
reduce b = foldM check b whole

check :: Board -> (V, I, I) -> Maybe Board
check b vij = case state b vij of
    Left []  -> Nothing
    Left [k] -> Just $ set b (vij, k)
    _        -> Just b

set :: Board -> ((V, I, I), I) -> Board
set b @ (s, u) vijk = (s', u - 1) where
    s'     = accum fix (accum remove s vijk's) (convert vijk)
    vijk's = do
        (vij, k) <- convert vijk
        k'       <- filter (/= k) . fromLeft $ state b vij
        convert (vij, k')

fix :: Either [I] I -> I -> Either [I] I
fix (Left ks) k = if k `elem` ks then Right k else error "fix: impossible choice"
fix (Right _) _ = error "fix: already fixed"

remove :: Either [I] I -> I -> Either [I] I
remove (Left ks) k = Left $ delete k ks
remove (Right _) _ = error "remove: already fixed"

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

showBoard :: Board -> String
showBoard b = unlines [ [ aux i j | j <- whole ] | i <- whole ] where
    aux i j = either (const '.') showI $ state b (GRD, i, j)

showI :: I -> Char
showI = (['1' .. '9'] !!) . unI

readBoard :: String -> Board
readBoard s = foldl aux empty whole where
    aux b (i, j) = case readI $ lines s !! unI i !! unI j of
        Nothing -> b
        Just k  -> set b ((GRD, i, j), k)

readI :: Char -> Maybe I
readI c = lookup c [ (showI i, i) | i <- whole ]

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

fromLeft :: Either a b -> a
fromLeft = either id (error "fromLeft")
