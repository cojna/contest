{-# OPTIONS_GHC -O2 #-}

import           Control.Applicative
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM


main :: IO ()
main = do
    n <- readLn
    edges <- U.unfoldrN (n-1) (readInt3.B.dropWhile isSpace) <$> B.getContents
    print $ solve $ undirectedGraph n edges

solve :: Graph -> Cost
solve gr = diameter gr

readInt3 :: B.ByteString -> Maybe ((Int,Int,Int), B.ByteString)
readInt3 bs = Just ((x,y,z),bsz)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx
    Just (z, bsz) = B.readInt $ B.unsafeTail bsy

type Vertex = Int
type Cost = Int
type Edge = (Vertex, Vertex, Cost)
type Graph = V.Vector (U.Vector (Vertex, Cost))

directedGraph :: Int -> U.Vector Edge -> Graph
directedGraph numV edges = V.map U.fromList
    . V.unsafeAccumulate (flip (:)) (V.replicate numV [])
    . U.convert
    . U.map (\(src, dst, cost) -> (src, (dst, cost)))
    $ edges

undirectedGraph :: Int -> U.Vector Edge -> Graph
undirectedGraph numV edges = directedGraph numV
    $ edges U.++ U.map reverseEdge edges

vertices :: Graph -> U.Vector Vertex
vertices gr = U.generate (V.length gr) id

reverseEdge :: Edge -> Edge
reverseEdge (src, dst, cost) = (dst, src, cost)
{-# INLINE reverseEdge #-}

diameter :: Graph -> Cost
diameter tree = U.maximum
    . shortestPath tree
    . U.maxIndex
    $ shortestPath tree 0

shortestPath :: Graph -> Vertex -> U.Vector Cost
shortestPath tree root = U.create $ do
    dist <- UM.unsafeNew (V.length tree)
    UM.unsafeWrite dist root 0
    U.forM_ (V.unsafeIndex tree root) $ \(v, cost) ->
        fix `flip` root `flip` v `flip` cost $ \dfs p u c -> do
            UM.unsafeRead dist p >>= UM.unsafeWrite dist u . (+c)
            U.mapM_ (uncurry $ dfs u)
                . U.filter ((/=p).fst)
                $ V.unsafeIndex tree u
    return dist

