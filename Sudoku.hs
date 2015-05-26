import Control.Monad
import Data.Array.Unboxed
import Data.List
import Data.Tuple

main :: IO ()
main = interact $ unlines . map showGrid . lift solve . readGrid . concat . words

type Digit = Char

digits :: [Digit]
digits = "123456789"

blocks :: [[Digit]]
blocks = ["123", "456", "789"]

type Coord = (Digit, Digit, Digit)

cube :: (Coord, Coord)
cube = ((dMin, dMin, dMin), (dMax, dMax, dMax)) where
    dMin = minimum digits
    dMax = maximum digits

type Unit = Int

unit :: Array Unit [Coord]
unit = listArray (1, length list) list where
    list    = numbers ++ rows ++ columns ++ boxes
    numbers = [ triples [i] [j] digits | i <- digits , j <- digits ]
    rows    = [ triples [i] digits [d] | i <- digits , d <- digits ]
    columns = [ triples digits [j] [d] | j <- digits , d <- digits ]
    boxes   = [ triples is js [d] | is <- blocks , js <- blocks , d <- digits ]
    triples is js ds = [ (i, j, d) | i <- is , j <- js , d <- ds ]

owners :: Array Coord [Unit]
owners = accumArray (flip (:)) [] cube [ (c, u) | (u, cs) <- assocs unit , c <- cs ]

foes :: Array Coord [Coord]
foes = array cube [ (c, aux c us) | (c, us) <- assocs owners ] where
    aux c us = delete c $ nub $ concat [ unit ! u | u <- us ]

type Grid = (UArray Coord Bool, UArray Unit Int)

solve :: MonadPlus m => Grid -> m Grid
solve g = case minimumUnit g of
    Nothing -> return g
    Just u  -> msum [ lift solve $ assign g c | c <- unit ! u ]

minimumUnit :: Grid -> Maybe Unit
minimumUnit (_, o) = case map swap $ filter ((/= -1) . snd) $ assocs o of
    [] -> Nothing
    xs -> Just $ snd $ minimum xs

assign :: Grid -> Coord -> Maybe Grid
assign (p, o) c0
    | p ! c0
        = foldM check (p', o') us
    | otherwise
        = mzero
    where
        cs = [ c | c <- foes ! c0 , p ! c ]
        p' = p // [ (c, False) | c <- cs ]
        o' = accum (-) o xs // ys
        xs = [ (u, 1) | c <- cs , u <- owners ! c ]
        ys = [ (u, -1) | u <- owners ! c0 ]
        us = concat [ owners ! c | c <- cs ]

check :: Grid -> Unit -> Maybe Grid
check g @ (p, o) u = case o ! u of
    0 -> mzero
    1 -> assign g $ head $ filter (p !) $ unit ! u
    _ -> return g

emptyGrid :: Grid
emptyGrid = (constArray cube True, constArray (bounds unit) $ length digits) where
    constArray bds v = array bds [ (k, v) | k <- range bds ]

readGrid :: String -> Maybe Grid
readGrid = foldM aux emptyGrid . zip ijs . concat . lines where
    ijs = [ (i, j) | i <- digits , j <- digits ]
    aux g ((i, j), d)
        | d `elem` digits
            = assign g (i, j, d)
        | otherwise
            = return g

showGrid :: Grid -> String
showGrid (p, _) = unlines [ [ aux i j | j <- digits ] | i <- digits ] where
    aux i j = case [ d | d <- digits , p ! (i, j, d) ] of
        [d] -> d
        _   -> '.'

lift :: MonadPlus m => (a -> m b) -> Maybe a -> m b
lift = maybe mzero
