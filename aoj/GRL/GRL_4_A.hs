{-# OPTIONS_GHC -O2 #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import           Data.Primitive.MutVar
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [v, e] <- map read.words <$> getLine
    edges <- U.unfoldrN e (readInt2.B.dropWhile isSpace) <$> B.getContents
    print . fromEnum . solve $ directedGraph v edges

solve :: Graph -> Bool
solve gr = hasCycle gr

------------------------------------------------------------------------------

readInt2 :: B.ByteString -> Maybe ((Int,Int), B.ByteString)
readInt2 bs = Just ((x,y),bsy)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb f = mb >>= flip when f
{-# INLINE whenM #-}

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb f = mb >>= flip unless f
{-# INLINE unlessM #-}

type Vertex = Int
type Edge = (Int, Int)
type Graph = V.Vector (U.Vector Vertex)

directedGraph :: Int -> U.Vector Edge -> Graph
directedGraph numV edges = V.map U.fromList
    . V.unsafeAccumulate (flip (:)) (V.replicate numV [])
    $ U.convert edges

vertices :: Graph -> U.Vector Vertex
vertices gr = U.generate (V.length gr) id

hasCycle :: Graph -> Bool
hasCycle gr = runST $ do
    visited <- UM.replicate (V.length gr) False
    onStack <- UM.replicate (V.length gr) False
    res <- newMutVar False
    U.forM_ (vertices gr) $ \root ->
        unlessM (UM.unsafeRead visited root) $
            flip fix root $ \dfs v -> do
                whenM (UM.unsafeRead onStack v) $
                    writeMutVar res True
                unlessM (UM.unsafeRead visited v) $ do
                    UM.unsafeWrite visited v True
                    UM.unsafeWrite onStack v True

                    U.mapM_ dfs $ V.unsafeIndex gr v

                    UM.unsafeWrite onStack v False

    readMutVar res


