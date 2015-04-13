import Data.Array.Unboxed
import Data.List
import Data.Maybe

main :: IO ()
main = interact (aux . solve . toSudoku) where
    aux ss
        = "found " ++ show (length ss) ++ " answers\n"
        ++ concat [ "\n" ++ show s | s <- ss ]

data Sudoku = Sudoku
    { definite :: Array (V, I, I) (Maybe I)
    , possible :: UArray C Bool
    }
    deriving Eq

data V = GRD | ROW | COL | BOX deriving (Bounded, Enum, Eq, Ix, Ord)

vs :: [V]
vs = [minBound .. maxBound]

type I = Int

is :: [I]
is = [0 .. n - 1]

type C = Int
cs :: [C]
cs = [0 .. n * n * n - 1]

n :: Int
n = m * m

m :: Int
m = 3

chars :: String
chars = ['1' .. '9']

toSudoku :: String -> Sudoku
toSudoku str = foldl aux s0  [ (i, j) | i <- is , j <- is ] where
    aux s (i, j) = case elemIndex (lines str !! i !! j) chars of
        Just k  -> fill s (toC GRD (i, j, k))
        Nothing -> s
    s0 = Sudoku { definite = d , possible = p }
    d = constArray ((minBound, 0, 0), (maxBound, n - 1, n - 1)) Nothing
    p = constArray (0, n * n * n - 1) True
    constArray bds val = array bds [ (i, val) | i <- range bds ]

instance Show Sudoku where
    show s = unlines [ [ toChar (definite s ! (GRD, i, j)) | j <- is ] | i <- is ] where
        toChar (Just k) = chars !! k
        toChar Nothing  = '.'

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

isComplete :: Sudoku -> Bool
isComplete s = and [ isJust (definite s ! (v, i, j)) | v <- vs , i <- is , j <- is ]

reduce :: Sudoku -> Maybe Sudoku
reduce s0 = foldl aux (Just s0) [ (v, i, j) | v <- vs , i <- is , j <- is ] where
    aux (Just s) (v, i, j)
        | isJust (definite s ! (v, i, j))
            = Just s
        | otherwise
            = case [ k | k <- is , possible s ! toC v (i, j, k) ] of
                []  -> Nothing
                [k] -> Just (fill s (toC v (i, j, k)))
                _   -> Just s
    aux Nothing _
        = Nothing

fill :: Sudoku -> C -> Sudoku
fill s c = s
    { definite = definite s // [ ((v, i, j), Just k) | v <- vs , let (i, j, k) = fromC v c ]
    , possible = possible s // [ (toC v (i, j, k'), False) | v <- vs , let (i, j, _) = fromC v c , k' <- is ]
    }

erase :: Sudoku -> C -> Sudoku
erase s c = s { possible = possible s // [ (c, False) ] }

toC :: V -> (I, I, I) -> C
toC GRD (i, j, k) = i * n * n + j * n + k
toC ROW (i, j, k) = j * n * n + k * n + i
toC COL (i, j, k) = k * n * n + j * n + i
toC BOX (i, j, k) = (j `div` m * m + k `div` m) * n * n + (j `mod` m * m + k `mod` m) * n + i

fromC :: V -> C -> (I, I, I)
fromC v c = case v of
    GRD -> (i, j, k)
    ROW -> (k, i, j)
    COL -> (k, j, i)
    BOX -> (k, i `div` m * m + j `div` m, i `mod` m * m + j `mod` m)
    where
        i = c `div` n `div` n
        j = c `div` n `mod` n
        k = c `mod` n
