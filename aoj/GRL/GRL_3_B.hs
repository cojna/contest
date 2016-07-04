{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Primitive.MutVar
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [v, e] <- map read.words <$> getLine
    edges <- U.unfoldrN e (readInt2.B.dropWhile isSpace) <$> B.getContents
    putStr.unlines.map showEdge . solve . fromEdges v $ toUndirectedEdges edges

solve :: Graph -> [Edge]
solve gr = sort $ bridges gr

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = V.Vector (U.Vector Vertex)

showEdge :: Edge -> String
showEdge (src, dst) = shows src " " ++ show dst

toUndirectedEdges :: U.Vector Edge -> U.Vector Edge
toUndirectedEdges edges = edges U.++ U.map swap edges

fromEdges :: Int -> U.Vector Edge -> Graph
fromEdges numV edges = V.map U.fromList
    . V.unsafeAccumulate (flip (:)) (V.replicate numV [])
    $ U.convert edges

nothing :: Int
nothing = -1

bridges :: Graph -> [Edge]
bridges gr = runST $ do
    let numV = V.length gr
    low <- UM.replicate numV nothing
    num <- UM.replicate numV nothing
    parent <- UM.replicate numV nothing
    bridge <- newMutVar [] :: ST s (MutVar s [Edge])
    numOfVisited <- newMutVar 0 :: ST s (MutVar s Int)

    let root = 0

    fix `flip` root `flip` nothing $ \dfs !v !p -> do
        !nv <- readMutVar numOfVisited
        modifyMutVar' numOfVisited (+1)
        UM.unsafeWrite num v nv
        UM.unsafeWrite parent v p
        UM.unsafeWrite low v nv

        U.forM_ (V.unsafeIndex gr v) $ \child -> do
            isVisited <- (/= nothing) <$> UM.unsafeRead num child
            if not isVisited
            then do
                dfs child v
                lc <- UM.unsafeRead low child
                unsafeModify low v (min lc)
                when (nv < lc) $
                    modifyMutVar' bridge ((min v child, max v child):)
            else
                when (child /= p) $ do
                    nc <- UM.unsafeRead num child
                    unsafeModify low v (min nc)

    readMutVar bridge

-------------------------------------------------------------------------------

readInt2 :: B.ByteString -> Maybe ((Int,Int), B.ByteString)
readInt2 bs = Just ((x,y),bsy)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx

unsafeModify :: (PrimMonad m, GM.MVector mv a)
             => mv (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeModify v i f = GM.unsafeRead v i >>= GM.unsafeWrite v i . f
{-# INLINE unsafeModify #-}