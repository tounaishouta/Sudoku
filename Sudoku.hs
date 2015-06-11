import Control.Monad
import Data.Array.Unboxed
import Data.List

main :: IO ()
main = interact $ unlines . map showGrid . solve <=< readGrid

type Digit = Char

type Coord = (Digit , Digit , Digit)

type Block = Int

type Grid = (UArray Coord Bool , UArray Block Int)

defined :: Int
defined = maxBound

groups :: [[Digit]]
groups = ["123" , "456" , "789"]

digits :: [Digit]
digits = concat groups

boundsOfCoord :: (Coord , Coord)
boundsOfCoord = ((dMin , dMin , dMin) , (dMax , dMax , dMax)) where
    dMin = minimum digits
    dMax = maximum digits

boundsOfBlock :: (Block , Block)
boundsOfBlock = bounds members

members :: Array Block [Coord]
members = listArray (1 , length list) list where
    list    = numbers ++ rows ++ columns ++ boxes
    numbers = [ triples [i] [j] digits | i <- digits , j <- digits ]
    rows    = [ triples [i] digits [d] | i <- digits , d <- digits ]
    columns = [ triples digits [j] [d] | j <- digits , d <- digits ]
    boxes   = [ triples is js [d] | is <- groups , js <- groups , d <- digits ]
    triples is js ds = [ (i , j , d) | i <- is , j <- js , d <- ds ]

owners :: Array Coord [Block]
owners = accumArray add [] boundsOfCoord cbs where
    add cs c = cs ++ [c]
    cbs = [ (c , b) | (b , cs) <- assocs members , c <- cs ]

antis :: Array Coord [Coord]
antis = array boundsOfCoord [ (c , aux c bs) | (c , bs) <- assocs owners ] where
    aux c bs = delete c $ nub $ concat [ members ! b | b <- bs ]

solve :: MonadPlus m => Grid -> m Grid
solve g @ (_ , o)
    | n == defined
        = return g
    | otherwise
        = msum [ solve =<< assign g c | c <- members ! b ]
    where (n , b) = minimum [ (n' , b') | (b' , n') <- assocs o ]

assign :: MonadPlus m => Grid -> Coord -> m Grid
assign (p , o) c
    | p ! c
        = foldM check (p' , o') bs
    | otherwise
        = mzero
    where
        cs = [ c' | c' <- antis ! c , p ! c' ]
        bs = [ b | c' <- cs , b <- owners ! c' ]
        p' = p // [ (c' , False) | c' <- cs ]
        o' = accum (-) o [ (b , 1) | b <- bs ] // [ (b , defined) | b <- owners ! c ]

check :: MonadPlus m => Grid -> Block -> m Grid
check g @ (p , o) b = case o ! b of
    0 -> mzero
    1 -> assign g c
    _ -> return g
    where c = head [ c' | c' <- members ! b , p ! c' ]

showGrid :: Grid -> String
showGrid (p , _) = unlines [ [ aux i j | j <- digits ] | i <- digits ] where
    aux i j = case [ d | d <- digits , p ! (i , j , d) ] of
        [d] -> d
        _   -> '.'

readGrid :: MonadPlus m => String -> m Grid
readGrid = foldM aux emptyGrid . zip ijs . concat . words where
    ijs = [ (i , j) | i <- digits , j <- digits ]
    aux g ((i , j) , d)
        | d `elem` digits
            = assign g (i , j , d)
        | otherwise
            = return g

emptyGrid :: Grid
emptyGrid = (p , o) where
    p = array boundsOfCoord [ (c , True) | c <- range boundsOfCoord ]
    o = array boundsOfBlock [ (b , length digits) | b <- range boundsOfBlock ]
