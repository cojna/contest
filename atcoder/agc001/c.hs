{-# LANGUAGE BangPatterns #-}

import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Unsafe     as B
import           Data.Char
import           Data.Tuple
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as U

main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine :: IO [Int]
    edges <- U.map(\(x,y)->(x-1,y-1)).U.unfoldrN n parseInt2 <$> B.getContents
    print $ solve n k edges

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = V.Vector (U.Vector Vertex)

toUndirectedEdges :: U.Vector Edge -> U.Vector Edge
toUndirectedEdges edges = edges U.++ U.map swap edges

fromEdges :: Int -> U.Vector Edge -> Graph
fromEdges numV edges = V.map U.fromList
    . V.unsafeAccumulate (flip (:)) (V.replicate numV [])
    $ U.convert edges

nothing :: Int
nothing = -1

solve :: Int -> Int -> U.Vector (Int, Int) -> Int
solve n k edges
    | even k = n - U.maximum (U.generate n (dfs (div k 2) nothing))
    | otherwise = n - U.maximum (U.map (\(v, u)->dfs (div (k-1) 2) v u + dfs (div (k-1) 2) u v) edges)
  where
    !gr = fromEdges n $ toUndirectedEdges edges
    dfs d prev v
        | d < 0 = 0
        | otherwise = U.foldr' ((+).dfs (d-1) v) 1
            . U.filter (/=prev)
            $ V.unsafeIndex gr v

-------------------------------------------------------------------------------

type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
