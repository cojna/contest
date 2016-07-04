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
    putStr . unlines . map show . solve $ directedGraph v edges

solve :: Graph -> [Vertex]
solve gr = U.toList $ topologicalSort gr

------------------------------------------------------------------------------

readInt2 :: B.ByteString -> Maybe ((Int,Int), B.ByteString)
readInt2 bs = Just ((x,y),bsy)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx

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

topologicalSort :: Graph -> U.Vector Vertex
topologicalSort gr = runST $ do
    visited <- UM.replicate (V.length gr) False
    sorted <- newMutVar []
    U.forM_ (vertices gr) . fix $ \dfs v ->
        unlessM (UM.unsafeRead visited v) $ do
            UM.unsafeWrite visited v True
            U.mapM_ dfs $ V.unsafeIndex gr v
            modifyMutVar' sorted (v:)
    U.fromList <$> readMutVar sorted
