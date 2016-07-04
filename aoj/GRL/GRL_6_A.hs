{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import           Data.Primitive.MutVar
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [v, e] <- map read.words <$> getLine
    edges <- U.unfoldrN e (readInt3.B.dropWhile isSpace) <$> B.getContents
    print $ solve v edges

solve :: Int -> U.Vector Edge -> Capacity
solve numV gr = maxFlow numV gr 0 (numV-1)

type Vertex = Int
type EdgeIndex = Int
type Capacity = Int
type Edge = (Vertex, Vertex, Capacity)
type Graph = V.Vector (U.Vector (Vertex, EdgeIndex))

inf :: Int
inf = 0x3f3f3f3f

nothing :: Int
nothing = -1

maxFlow :: Int -> U.Vector Edge -> Vertex -> Vertex -> Capacity
maxFlow numV edges source sink = runST $ do
    (gr, res) <- buildResidualGraph numV edges
    level <- UM.replicate (V.length gr) nothing
    iter <- UM.replicate (V.length gr) 0

    flip fix 0 $ \dinicLoop flow -> do
        maxFlowBFS gr res level source
        l <- UM.unsafeRead level sink
        if l == nothing then return flow
        else do
            UM.set iter 0
            flip fix flow $ \dfsLoop !acc -> do
                f <- maxFlowDFS gr res level iter source sink
                if f > 0
                then dfsLoop $ acc + f
                else dinicLoop acc

reverseEdgeIndex :: Int -> Int
reverseEdgeIndex = xor 1
{-# INLINE reverseEdgeIndex #-}

buildResidualGraph :: Int
                   -> U.Vector Edge
                   -> ST s (Graph, UM.MVector s Capacity)
buildResidualGraph numV edges = do
    gr <- VM.replicate numV []
    residual <- UM.replicate (2 * U.length edges) 0
    U.foldM'_ `flip` 0 `flip` edges $ \i (src, dst, cap) -> do
        unsafeModify gr src ((dst,i):) >> unsafeModify gr dst ((src,i+1):)
        UM.unsafeWrite residual i cap
        return $ i + 2
    flip (,) residual . V.map U.fromList <$> V.unsafeFreeze gr

maxFlowBFS :: Graph
           -> UM.MVector s Int
           -> UM.MVector s Int
           -> Vertex
           -> ST s ()
maxFlowBFS gr res level source = do
    UM.set level nothing
    queue <- newQueueM
    UM.unsafeWrite level source 0
    enqueueM source queue
    fix $ \bfs -> do
    hd <- dequeueM queue
    case hd of
        Just v -> do
            U.forM_ (V.unsafeIndex gr v) $ \(u, i) -> do
                lu <- UM.unsafeRead level u
                cap <- UM.unsafeRead res i
                when (cap > 0 && lu == nothing) $ do
                    lv <- UM.unsafeRead level v
                    UM.unsafeWrite level u $ lv + 1
                    enqueueM u queue
            bfs
        Nothing -> return ()

maxFlowDFS :: Graph
           -> UM.MVector s Int
           -> UM.MVector s Int
           -> UM.MVector s Int
           -> Vertex
           -> Vertex
           -> ST s Capacity
maxFlowDFS gr res level iter source sink =
    fix `flip` source `flip` sink `flip` inf $ \dfs !v !t !f ->
        if v == t then return f
        else do
            i0 <- UM.unsafeRead iter v
            flip fix (U.unsafeDrop i0 $ V.unsafeIndex gr v)$ \loop vec ->
                if U.length vec > 0
                then do
                    let (u, j) = U.unsafeHead vec
                    unsafeModify iter v (+1)
                    cap <- UM.unsafeRead res j
                    lv <- UM.unsafeRead level v
                    lu <- UM.unsafeRead level u
                    if cap > 0 && lv < lu
                    then do
                        d <- dfs u t (min f cap)
                        if d > 0
                        then do
                            unsafeModify res j (subtract d)
                            let k = reverseEdgeIndex j
                            unsafeModify res k (+d)
                            return d
                        else loop $ U.unsafeTail vec
                    else loop $ U.unsafeTail vec
                else return 0

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