import Control.Monad      (foldM)
import Data.Array.Unboxed (Ix, Array, UArray, array, range, (!), (//))
import Data.List          (intercalate, elemIndex)
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

is :: [I]
is = [0 .. n - 1]

type C = Int

cs :: [C]
cs = [0 .. n * n * n - 1]

chars :: String
chars = ['1' .. '9']

data Sudoku = Sudoku { definite :: Array (V, I, I) (Maybe I) , possible :: UArray C Bool } deriving (Eq)

instance Show Sudoku where
    show s = unlines [ [ toChar (definite s ! (GRD, i, j)) | j <- is ] | i <- is ] where
        toChar (Just k) = chars !! k
        toChar Nothing  = '.'

instance Read Sudoku where
    readsPrec _ input = [(s0, unlines (drop n lns))] where
        s0 = foldl aux empty [ (i, j) | i <- is , j <- is ]
        aux s (i, j) = case elemIndex (lns !! i !! j) chars of
            Just k  -> fill s (toC GRD i j k)
            Nothing -> s
        lns = lines input

empty :: Sudoku
empty = Sudoku { definite = d , possible = p } where
    d = constArray ((minBound, 0, 0), (maxBound, n - 1, n - 1)) Nothing
    p = constArray (0, n * n * n - 1) True
    constArray bds val = array bds [ (ind, val) | ind <- range bds ]

isComplete :: Sudoku -> Bool
isComplete s = and [ isJust (definite s ! (v, i, j)) | v <- vs , i <- is , j <- is ]

fill :: Sudoku -> C -> Sudoku
fill s c = s { definite = d , possible = p } where
    d = definite s // [ ((v, i, j), Just k) | (v, i, j, k) <- fromC c ]
    p = possible s // [ (toC v i j k', False) | (v, i, j, _) <- fromC c , k' <- is ]

erase :: Sudoku -> C -> Sudoku
erase s c = s { possible = possible s // [(c, False)] }

reduce :: Sudoku -> Maybe Sudoku
reduce s0 = foldM aux s0 [ (v, i, j) | v <- vs , i <- is , j <- is ] where
    aux s (v, i, j)
        | isJust (definite s ! (v, i, j))
            = Just s
        | otherwise
            = case [ k | k <- is , possible s ! toC v i j k ] of
                []  -> Nothing
                [k] -> Just (fill s (toC v i j k))
                _   -> Just s

solve :: Sudoku -> [Sudoku]
solve s
    | isComplete s
        = [s]
    | otherwise
        = case reduce s of
            Just s'
                | s == s'
                    -> solve (fill s c0) ++ solve (erase s c0)
                | otherwise
                    -> solve s'
                where c0 = head [ c | c <- cs , possible s ! c ]
            Nothing
                -> []

toC :: V -> I -> I -> I -> C
toC GRD i j k = i * n * n + j * n + k
toC ROW i j k = j * n * n + k * n + i
toC COL i j k = k * n * n + j * n + i
toC BOX i j k = (j `div` m * m + k `div` m) * n * n + (j `mod` m * m + k `mod` m) * n + i

fromC :: C -> [(V, I, I, I)]
fromC c =
    [ (GRD, i, j, k)
    , (ROW, k, i, j)
    , (COL, k, j, i)
    , (BOX, k, i `div` m * m + j `div` m, i `mod` m * m + j `mod` m)
    ] where
        i = c `div` n `div` n
        j = c `div` n `mod` n
        k = c `mod` n
