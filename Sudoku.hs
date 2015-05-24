import Control.Monad      (MonadPlus, foldM, mzero, msum)
import Data.Array.Unboxed (Array, UArray, accum, accumArray, array, listArray, range, (!), (//))
import Data.List          (delete, intercalate, nub)

main :: IO ()
main = interact $ intercalate "\n" . map showGrid . solve . concat . lines

type Digit = Char

digits :: [Digit]
digits = "123456789"

boundsOfDigit :: (Digit, Digit)
boundsOfDigit = (minimum digits, maximum digits)

type Coord = (Digit, Digit, Digit)

boundsOfCoord :: (Coord, Coord)
boundsOfCoord = ((dMin, dMin, dMin), (dMax, dMax, dMax)) where
    (dMin, dMax) = boundsOfDigit

coords :: [Coord]
coords = range boundsOfCoord

type Unit = [Coord]

unitList :: [Unit]
unitList =  grids ++ rows ++ cols ++ boxes where
    grids = [ [ (d, i, j) | d <- digits ] | i <- digits , j <- digits ]
    rows  = [ [ (d, i, j) | j <- digits ] | d <- digits , i <- digits ]
    cols  = [ [ (d, i, j) | i <- digits ] | d <- digits , j <- digits ]
    boxes = [ [ (d, i, j) | i <- is , j <- js ] | d <- digits , is <- block , js <- block ]
    block = ["123", "456", "789"]

type UID = Int

boundsOfUID :: (UID, UID)
boundsOfUID = (1, length unitList)

uids :: [UID]
uids = range boundsOfUID

unit :: Array UID Unit
unit = listArray boundsOfUID unitList

containers :: Array Coord [UID]
containers = accumArray (flip (:)) [] boundsOfCoord [ (c, uid) | uid <- uids , c <- unit ! uid ]

foes :: Array Coord [Coord]
foes = array boundsOfCoord [ (c, aux c) | c <- coords ] where
    aux c = delete c $ nub $ concat [ unit ! uid | uid <- containers ! c ]

type Grid = (UArray Coord Bool, UArray UID Int)

admits :: Grid -> Coord -> Bool
admits = (!) . fst

numOfChoices :: Grid -> UID -> Int
numOfChoices = (!) . snd

solve :: MonadPlus m => String -> m Grid
solve = maybe mzero search . readGrid

search :: MonadPlus m => Grid -> m Grid
search g = case minimumUID g of
    Nothing  -> return g
    Just uid -> msum [ maybe mzero search $ assign g c | c <- unit ! uid ]

minimumUID :: Grid -> Maybe UID
minimumUID g
    | null xs
        = Nothing
    | otherwise
        = Just $ snd $ minimum xs
    where xs = filter ((> 1) . fst) [ (numOfChoices g uid, uid) | uid <- uids ]

assign :: Grid -> Coord -> Maybe Grid
assign g c
    | g `admits` c
        = foldM eliminate g $ foes ! c
    | otherwise
        = Nothing

eliminate :: Grid -> Coord -> Maybe Grid
eliminate g @ (b, n) c
    | g `admits` c
        = foldM check (b', n') $ containers ! c
    | otherwise
        = Just g
    where
        b' = b // [(c, False)]
        n' = accum (-) n [ (uid, 1) | uid <- containers ! c ]

check :: Grid -> UID -> Maybe Grid
check g uid = case numOfChoices g uid of
    0 -> Nothing
    1 -> assign g c
    _ -> Just g
    where c = head $ filter (admits g) $ unit ! uid

showGrid :: Grid -> String
showGrid g = unlines [ [ aux i j | j <- digits ] | i <- digits ] where
    aux i j = case [ d | d <- digits , g `admits` (d, i, j) ] of
        []  -> 'x'
        [d] -> d
        _   -> '.'

readGrid :: String -> Maybe Grid
readGrid s = foldM aux emptyGrid $ zip s [ (i, j) | i <- digits , j <- digits ] where
    aux g (d, (i, j))
        | d `elem` digits
            = assign g (d, i, j)
        | otherwise
            = Just g

emptyGrid :: Grid
emptyGrid = (constArray boundsOfCoord True, constArray boundsOfUID (length digits)) where
    constArray bds v = array bds [ (k, v) | k <- range bds ]
