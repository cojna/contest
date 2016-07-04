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
import           Data.Primitive.MutVar
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [v, e, r] <- map read.words <$> getLine
    edges <- U.unfoldrN e (readInt3.B.dropWhile isSpace) <$> B.getContents
    putStr.maybe"NEGATIVE CYCLE\n"(unlines.map showCost.U.toList).solve r $ fromEdges v edges

solve :: Vertex -> Graph -> Maybe (U.Vector Cost)
solve root gr = spfa root gr

showCost :: Cost -> String
showCost cost
    | cost < inf = show cost
    | otherwise = "INF"

-------------------------------------------------------------------------------

readInt3 :: B.ByteString -> Maybe ((Int,Int,Int), B.ByteString)
readInt3 bs = Just ((x,y,z),bsz)
  where
    Just (x, bsx) = B.readInt bs
    Just (y, bsy) = B.readInt $ B.unsafeTail bsx
    Just (z, bsz) = B.readInt $ B.unsafeTail bsy

unsafeModify :: (PrimMonad m, GM.MVector mv a)
             => mv (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeModify v i f = GM.unsafeRead v i >>= GM.unsafeWrite v i . f
{-# INLINE unsafeModify #-}

type Vertex = Int
type Cost = Int
type Edge = (Vertex, Vertex, Cost)
type Graph = V.Vector (U.Vector (Vertex, Cost))

fromEdges :: Int -> U.Vector Edge -> Graph
fromEdges numV edges = V.map U.fromList
    . V.unsafeAccumulate (flip (:)) (V.replicate numV [])
    . U.convert
    . U.map (\(src, dst, cost) -> (src, (dst, cost)))
    $ edges

inf :: Cost
inf = 0x3f3f3f3f

spfa :: Vertex -> Graph -> Maybe (U.Vector Cost)
spfa root gr = runST $ do
    let !numV = V.length gr
    dist <- UM.replicate numV inf
    inQueue <- UM.replicate numV False
    visit <- UM.replicate numV 0
    queue <- newQueueM

    UM.unsafeWrite dist root 0
    UM.unsafeWrite inQueue root True
    unsafeModify visit root (+1)
    enqueueM root queue

    fix $ \loop -> do
        hd <- dequeueM queue
        case hd of
            Just v -> do
                UM.unsafeWrite inQueue v False
                count <- UM.unsafeRead visit v
                if count <= numV
                then do
                    dv <- UM.unsafeRead dist v
                    U.forM_ (V.unsafeIndex gr v) $ \(nv, v2nv) -> do
                        let !dnv = dv + v2nv
                        old <- UM.unsafeRead dist nv
                        when (dnv < old) $ do
                            UM.unsafeWrite dist nv dnv
                            inQ <- UM.unsafeRead inQueue nv
                            unless inQ $ do
                                UM.unsafeWrite inQueue nv True
                                unsafeModify visit nv (+1)
                                enqueueM nv queue
                    loop
                else return Nothing
            Nothing -> Just <$> U.unsafeFreeze dist


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
