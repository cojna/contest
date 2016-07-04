{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import           Data.Primitive.MutVar
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [v, e] <- map read.words <$> getLine
    edges <- U.replicateM e $ do
        st <- U.unfoldrN 2 (B.readInt.B.dropWhile isSpace) <$> B.getLine
        return (U.unsafeIndex st 0, U.unsafeIndex st 1)
    q <- readLn
    queries <- U.unfoldrN q (readInt2.B.dropWhile isSpace) <$> B.getContents
    putStr.unlines.map (bool "0" "1"). solve queries $ directedGraph v edges

solve :: U.Vector (Vertex, Vertex) -> Graph -> [Bool]
solve qs gr = U.toList $ U.map (uncurry query) qs
  where
    !scc = V.fromList $ stronglyConnectedComponents gr
    !table = U.unsafeUpdate (U.replicate (V.length gr) nothing)
        . U.concatMap (\i -> U.map (flip (,) i) $ V.unsafeIndex scc i)
        $ U.generate (V.length scc) id
    query = (==) `on` U.unsafeIndex table

------------------------------------------------------------------------------

readInt2 :: B.ByteString -> Maybe ((Int,Int), B.ByteString)
readInt2 bs = Just ((x,y),bsy)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb f = mb >>= flip when f
{-# INLINE whenM #-}

unsafeModify :: (PrimMonad m, GM.MVector mv a)
             => mv (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeModify v i f = GM.unsafeRead v i >>= GM.unsafeWrite v i . f
{-# INLINE unsafeModify #-}

type Vertex = Int
type Edge = (Int, Int)
type Graph = V.Vector (U.Vector Vertex)

directedGraph :: Int -> U.Vector Edge -> Graph
directedGraph numV edges = V.map U.fromList
    . V.unsafeAccumulate (flip (:)) (V.replicate numV [])
    $ U.convert edges

vertices :: Graph -> U.Vector Vertex
vertices gr = U.generate (V.length gr) id

nothing :: Int
nothing = -1

stronglyConnectedComponents :: Graph -> [U.Vector Vertex]
stronglyConnectedComponents gr = runST $ do
    let numV = V.length gr
    low <- UM.replicate numV nothing
    preord <- UM.replicate numV nothing
    stack <- newMutVar []
    onStack <- UM.replicate numV False
    num <- newMutVar 0
    components <- newMutVar []

    U.forM_ (vertices gr) $ \root ->
        whenM ((==nothing) <$> UM.unsafeRead preord root) $
            flip fix root $ \dfs v -> do
                pord <- readMutVar num <* modifyMutVar' num (+1)
                UM.unsafeWrite preord v pord >> UM.unsafeWrite low v pord

                modifyMutVar' stack (v:) >> UM.unsafeWrite onStack v True

                U.forM_ (V.unsafeIndex gr v) $ \u -> do
                    isVisited <- (/= nothing) <$> UM.unsafeRead preord u
                    if isVisited
                    then whenM (UM.unsafeRead onStack u) $
                        UM.unsafeRead preord u >>= unsafeModify low v . min
                    else do
                        dfs u
                        UM.unsafeRead low u >>= unsafeModify low v . min

                isRoot <- (==)
                    <$> UM.unsafeRead low v
                    <*> UM.unsafeRead preord v
                when isRoot $ do
                    (vs, u:us) <- span (/=v) <$> readMutVar stack
                    let !component = U.fromList $ u:vs
                    modifyMutVar' components (component:)
                    writeMutVar stack us
                    U.mapM_ (flip (UM.unsafeWrite onStack) False) component

    readMutVar components

