import Control.Monad
import Data.Array.Unboxed
import Data.List
import Data.Tuple

main :: IO ()
main = interact $ unlines . map showGrid . liftMaybe solve . readGrid

type Digit = Char

digitss :: [[Digit]]
digitss = ["123", "456", "789"]

digits :: [Digit]
digits = concat digitss

type Coord = (Digit, Digit, Digit)

boundsOfCoord :: (Coord, Coord)
boundsOfCoord = ((dMin, dMin, dMin), (dMax, dMax, dMax)) where
    dMin = minimum digits
    dMax = maximum digits

type Block = Int

coords :: Array Block [Coord]
coords = listArray (1, length blocks) blocks where
    blocks  = grids ++ rows ++ columns ++ boxes
    grids   = [ triples [i] [j] digits | i <- digits , j <- digits ]
    rows    = [ triples [i] digits [d] | i <- digits , d <- digits ]
    columns = [ triples digits [j] [d] | j <- digits , d <- digits ]
    boxes   = [ triples is js [d] | is <- digitss , js <- digitss , d <- digits ]
    triples is js ds = [ (i, j, d) | i <- is , j <- js , d <- ds ]

owners :: Array Coord [Block]
owners = accumArray append [] boundsOfCoord [ (c, b) | (b, cs) <- assocs coords , c <- cs ]
    where append cs c = cs ++ [c]

others :: Array Coord [Coord]
others = array boundsOfCoord [ (c, aux c bs) | (c, bs) <- assocs owners ]
    where aux c bs = delete c $ nub $ concat [ coords ! b | b <- bs ]

boundsOfBlock :: (Block, Block)
boundsOfBlock = bounds coords

type Grid = (UArray Coord Bool, UArray Block Int)

defined :: Int
defined = maxBound

showGrid :: Grid -> String
showGrid (p, _) = unlines [ [ aux i j | j <- digits ] | i <- digits ] where
    aux i j = case [ d | d <- digits , p ! (i, j, d) ] of
        [d] -> d
        _   -> '.'

liftMaybe :: MonadPlus m => (a -> m b) -> Maybe a -> m b
liftMaybe = maybe mzero

solve :: MonadPlus m => Grid -> m Grid
solve g @ (_, o)
    | n == defined
        = return g
    | otherwise
        = msum [ liftMaybe solve $ assign g c | c <- coords ! b ]
    where (n, b) = minimum $ map swap $ assocs o

readGrid :: String -> Maybe Grid
readGrid = foldM aux emptyGrid . zip ijs . concat . words where
    ijs = [ (i, j) | i <- digits , j <- digits ]
    aux g ((i, j), d)
        | d `elem` digits
            = assign g (i, j, d)
        | otherwise
            = return g

assign :: Grid -> Coord -> Maybe Grid
assign (p, o) c
    | p ! c
        = foldM check (p', o') bs
    | otherwise
        = mzero
    where
        cs = [ c' | c' <- others ! c , p ! c' ]
        bs = [ b | c' <- cs , b <- owners ! c' ]
        p' = p // [ (c', False) | c' <- cs ]
        o' = accum (-) o [ (b, 1) | b <- bs ] // [ (b, defined) | b <- owners ! c ]

check :: Grid -> Block -> Maybe Grid
check g @ (p, o) b = case o ! b of
    0 -> mzero
    1 -> assign g c
    _ -> return g
    where c = head $ filter (p !) $ coords ! b

emptyGrid :: Grid
emptyGrid = (p, o) where
    p = constArray boundsOfCoord True
    o = constArray boundsOfBlock $ length digits
    constArray bds v = array bds [ (k, v) | k <- range bds ]
