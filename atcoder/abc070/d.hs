{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           Unsafe.Coerce

main :: IO ()
main = do
    !n <- readLn :: IO Int
    is <- U.unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getContents
    let abcs = U.toList $ U.take (3*pred n) is
    let (_:k:xys) = U.toList $ U.drop (3*pred n) is
    putStr.unlines.map show $ solve n (k-1) (parse3 abcs) $ parse2 xys

parse3 (x:y:z:xs) = (x-1,y-1,z):parse3 xs
parse3 _ = []

parse2 (x:y:xs) = (x-1,y-1):parse2 xs
parse2 _ = []

solve :: Int -> Int -> [(Int, Int, Int)] -> [(Int, Int)] -> [Int]
solve n k abcs xys = map (\(x,y)->((+)`on`U.unsafeIndex dist) x y) xys
  where
    !gr = fromEdges n.toUndirectedEdges . U.fromList $  abcs
    !dist = bfs k gr

bfs :: Vertex -> Graph -> U.Vector Cost
bfs root gr = runST $ do
    dist <- UM.replicate (V.length gr) (maxBound :: Cost)
    UM.unsafeWrite dist root 0
    queue <- newQueueM
    enqueueM (root, 0) queue
    fix $ \loop -> do
        front <- dequeueM queue
        case front of
            Just (f, d) -> do
                U.forM_ (V.unsafeIndex gr f) $ \(v, c) -> do
                    dv <- UM.unsafeRead dist v
                    when (d + c < dv) $ do
                        UM.unsafeWrite dist v (d + c)
                        enqueueM (v, d + c) queue
                        loop
            Nothing -> return ()
    U.unsafeFreeze dist

-------------------------------------------------------------------------------
data QueueM m a = QM (MutVar m [a]) (MutVar m [a])

newQueueM:: (PrimMonad m) => m (QueueM (PrimState m) a)
newQueueM = QM `liftM` newMutVar [] `ap` newMutVar []

dequeueM :: (PrimMonad m) => QueueM (PrimState m) a -> m (Maybe a)
dequeueM (QM front rear) = do
  fs <- readMutVar front
  case fs of
    (f:fs') -> writeMutVar front fs' >> return (Just f)
    [] -> do
      rs <- readMutVar rear
      writeMutVar rear []
      case reverse rs of
        (r:rs') -> writeMutVar front rs' >> return (Just r)
        [] -> return Nothing
{-# INLINE dequeueM #-}

enqueueM :: (PrimMonad m) => a -> QueueM (PrimState m) a -> m ()
enqueueM x (QM _ rear) = modifyMutVar' rear (x:)
{-# INLINE enqueueM #-}

type Vertex = Int
type Cost = Int
type Edge = (Vertex, Vertex, Cost)
type Graph = V.Vector (U.Vector (Vertex, Cost))

reverseEdge :: Edge -> Edge
reverseEdge (src, dst, cost) = (dst, src, cost)

toUndirectedEdges :: U.Vector Edge -> U.Vector Edge
toUndirectedEdges edges = edges U.++ U.map reverseEdge edges

fromEdges :: Int -> U.Vector Edge -> Graph
fromEdges numV edges = V.map U.fromList
    . V.unsafeAccumulate (flip (:)) (V.replicate numV [])
    . U.convert
    . U.map (\(src, dst, cost) -> (src, (dst, cost)))
    $ edges

reverseGraph :: Graph -> Graph
reverseGraph gr = V.map U.fromList
    . V.unsafeAccumulate (flip (:)) (V.replicate (V.length gr) [])
    . V.concatMap (V.map (\(src, (dst, cost)) -> (dst, (src, cost))))
    . V.map U.convert
    . V.zipWith (\i adj -> U.map ((,) i) adj) (V.generate (V.length gr) id)
    $ gr
