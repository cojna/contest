{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, LambdaCase, MagicHash                   #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings       #-}
{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, ViewPatterns #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString             as B
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Internal    as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Foldable               as F
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    pairs <- U.map (\(x,y) -> (x - 1, y - 1)) . U.unfoldrN (m + 1) parseInt2 <$> C.getContents
    let (s, t) = U.last pairs
    print $ solveByBFS n s t $ U.take m pairs

#define INF 0x3f3f3f3f3f3f3f3f

solveByBFS :: Int -> Int -> Int -> U.Vector (Int, Int) -> Int
solveByBFS n s t edges0
    | dist U.! (3 * t) >= INF = -1
    | otherwise = dist U.! (3 * t) `div` 3
  where
    edges01, edges12, edges20 :: U.Vector Edge
    edges01 = U.map (\(x, y) -> (3 * x, 3 * y + 1)) edges0
    edges12 = U.map (\(x, y) -> (3 * x + 1, 3 * y + 2)) edges0
    edges20 = U.map (\(x, y) -> (3 * x + 2, 3 * y)) edges0
    gr = buildDirectedGraph (3 * n)
        $ U.concat [edges01, edges12, edges20]
    dist = bfsCSR (3 * s) gr

solveByDijkstra :: Int -> Int -> Int -> U.Vector (Int, Int) -> Int
solveByDijkstra n s t edges0
    | dist U.! (3 * t) >= INF = -1
    | otherwise = dist U.! (3 * t)
  where
    edges01, edges12, edges20 :: U.Vector (EdgeWith Int)
    edges01 = U.map (\(x, y) -> (3 * x, 3 * y + 1, 0)) edges0
    edges12 = U.map (\(x, y) -> (3 * x + 1, 3 * y + 2, 0)) edges0
    edges20 = U.map (\(x, y) -> (3 * x + 2, 3 * y, 1)) edges0
    gr = buildDirectedGraphWithE (3 * n)
        $ U.concat [edges01, edges12, edges20]
    dist = dijkstraCSR (3 * s) gr

type Vertex = Int
type Edge = (Vertex, Vertex)
type EdgeWith w = (Vertex, Vertex, w)
data CompressedSparseRowGraph w = CSR
    { numVerticesCSR :: !Int
    , numEdgesCSR    :: !Int
    , offsetCSR      :: !(U.Vector Int)
    , adjacentCSR    :: !(U.Vector Vertex)
    , edgeCtxCSR     :: !(U.Vector w)
    }

buildDirectedGraph
    :: Int -> U.Vector Edge -> CompressedSparseRowGraph ()
buildDirectedGraph numVerticesCSR edges = runST $ do
    let numEdgesCSR = U.length edges
    let offsetCSR = U.scanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate numVerticesCSR 0)
            . U.map (flip (,) 1)
            . fst
            $ U.unzip edges
    moffset <- U.thaw offsetCSR
    madj <- UM.new numEdgesCSR
    U.forM_ edges $ \(src, dst) -> do
        pos <- UM.unsafeRead moffset src
        UM.unsafeWrite moffset src (pos + 1)
        UM.unsafeWrite madj pos dst
    adjacentCSR <- U.unsafeFreeze madj
    return CSR{edgeCtxCSR = U.replicate numEdgesCSR (), ..}

buildDirectedGraphWithE :: (U.Unbox w)
    => Int -> U.Vector (EdgeWith w) -> CompressedSparseRowGraph w
buildDirectedGraphWithE numVerticesCSR edges = runST $ do
    let numEdgesCSR = U.length edges
    let offsetCSR = U.scanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate numVerticesCSR 0)
            . U.map (flip (,) 1)
            . (\(x, _, _) -> x)
            $ U.unzip3 edges
    moffset <- U.thaw offsetCSR
    madj <- UM.new numEdgesCSR
    mectx <- UM.new numEdgesCSR
    U.forM_ edges $ \(src, dst, w) -> do
        pos <- UM.unsafeRead moffset src
        UM.unsafeWrite moffset src (pos + 1)
        UM.unsafeWrite madj pos dst
        UM.unsafeWrite mectx pos w
    adjacentCSR <- U.unsafeFreeze madj
    edgeCtxCSR <- U.unsafeFreeze mectx
    return CSR{..}

adj :: CompressedSparseRowGraph w -> Vertex -> U.Vector Vertex
adj CSR{..} v = U.unsafeSlice o (o' - o) adjacentCSR
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE adj #-}

adjWithE :: (U.Unbox w)
    => CompressedSparseRowGraph w -> Vertex -> U.Vector (Vertex, w)
adjWithE CSR{..} v = U.zip
    (U.unsafeSlice o (o' - o) adjacentCSR)
    (U.unsafeSlice o (o' - o) edgeCtxCSR)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE adjWithE #-}

bfsCSR :: (U.Unbox w) => Vertex -> CompressedSparseRowGraph w -> U.Vector Int
bfsCSR source gr@CSR{..} = U.create $ do
    dist <- UM.replicate numVerticesCSR INF
    que <- withCapacityQ numEdgesCSR
    UM.write dist source 0
    enqueue source que
    fix $ \loop -> do
        dequeue que >>= \case
            Just v -> do
                dv <- UM.unsafeRead dist v
                U.forM_ (gr `adj` v) $ \v' -> do
                    visited <- (< INF) <$!> UM.unsafeRead dist v'
                    unless visited $ do
                        UM.unsafeWrite dist v' $ dv + 1
                        enqueue v' que
                loop
            Nothing -> return ()
    return dist

dijkstraCSR :: (U.Unbox w, Num w, Ord w)
    => Vertex -> CompressedSparseRowGraph w -> U.Vector w
dijkstraCSR source gr@CSR{..} = U.create $ do
    dist <- UM.replicate numVerticesCSR INF
    heap <- newBinaryHeap numEdgesCSR
    UM.write dist source 0
    insertMinBH (0, source) heap
    fix $ \loop -> do
        deleteFindMinBH heap >>= \case
            Just (d, v) -> do
                dv <- UM.unsafeRead dist v
                when (dv == d) $ do
                    U.forM_ (gr `adjWithE` v) $ \(v', w') -> do
                        dv' <- UM.unsafeRead dist v'
                        when (dv + w' < dv') $ do
                            UM.unsafeWrite dist v' $ dv + w'
                            insertMinBH (dv + w', v') heap
                loop
            Nothing -> return ()
    return dist
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
-------------------------------------------------------------------------------
-- Data.Heap.BinaryHeap.Min
-------------------------------------------------------------------------------
data BinaryHeap s a = BinaryHeap (MutVar s Int) (UM.MVector s a)
newBinaryHeap :: (PrimMonad m, U.Unbox a) => Int -> m (BinaryHeap (PrimState m) a)
newBinaryHeap n = BinaryHeap <$> newMutVar 0 <*> UM.new n
getBinaryHeapSize :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m Int
getBinaryHeapSize (BinaryHeap ref _) = readMutVar ref
{-# INLINE getBinaryHeapSize #-}
siftUp :: (PrimMonad m, U.Unbox a, Ord a) => Int -> UM.MVector (PrimState m) a -> m ()
siftUp k vec = do { x <- UM.unsafeRead vec k; flip fix k $ \ loop !i -> if i > 0 then do { let { !parent = (i - 1) `unsafeShiftR` 1}; p <- UM.unsafeRead vec parent; if p <= x then UM.unsafeWrite vec i x else do { UM.unsafeWrite vec i p; loop parent}} else UM.unsafeWrite vec 0 x}
{-# INLINE siftUp #-}
siftDown :: (PrimMonad m, U.Unbox a, Ord a) => Int -> UM.MVector (PrimState m) a -> m ()
siftDown k vec = do { x <- UM.unsafeRead vec k; let { n = UM.length vec}; flip fix k $ \ loop !i -> do { let { !l = unsafeShiftL i 1 .|. 1}; if n <= l then UM.unsafeWrite vec i x else do { let { !r = l + 1}; childL <- UM.unsafeRead vec l; childR <- UM.unsafeRead vec r; if r < n && childR < childL then if x <= childR then UM.unsafeWrite vec i x else do { UM.unsafeWrite vec i childR; loop r} else if x <= childL then UM.unsafeWrite vec i x else do { UM.unsafeWrite vec i childL; loop l}}}}
{-# INLINE siftDown #-}
heapify :: (PrimMonad m, U.Unbox a, Ord a) => UM.MVector (PrimState m) a -> m ()
heapify vec = do { rev (UM.length vec `quot` 2) $ \ i -> do { siftDown i vec}}
{-# INLINE heapify #-}
buildBinaryHeap :: (PrimMonad m, U.Unbox a, Ord a) => U.Vector a -> m (BinaryHeap (PrimState m) a)
buildBinaryHeap vec = do { ref <- newMutVar $ U.length vec; mvec <- U.unsafeThaw vec; heapify mvec; return $! BinaryHeap ref mvec}
{-# INLINE buildBinaryHeap #-}
unsafeMinViewBH :: (PrimMonad m, U.Unbox a) => BinaryHeap (PrimState m) a -> m a
unsafeMinViewBH (BinaryHeap _ vec) = UM.unsafeRead vec 0
{-# INLINE unsafeMinViewBH #-}
minViewBH :: (PrimMonad m, U.Unbox a) => BinaryHeap (PrimState m) a -> m (Maybe a)
minViewBH bh = do { size <- getBinaryHeapSize bh; if size > 0 then Just <$!> unsafeMinViewBH bh else return $! Nothing}
{-# INLINE minViewBH #-}
insertMinBH :: (PrimMonad m, U.Unbox a, Ord a) => a -> BinaryHeap (PrimState m) a -> m ()
insertMinBH x bh@(BinaryHeap info vec) = do { size <- getBinaryHeapSize bh; modifyMutVar' info (+ 1); UM.unsafeWrite vec size x; siftUp size vec}
{-# INLINE insertMinBH #-}
unsafeDeleteMinBH :: (PrimMonad m, U.Unbox a, Ord a) => BinaryHeap (PrimState m) a -> m ()
unsafeDeleteMinBH bh@(BinaryHeap info vec) = do { size <- getBinaryHeapSize bh; modifyMutVar' info (subtract 1); UM.unsafeSwap vec 0 (size - 1); siftDown 0 (UM.unsafeTake (size - 1) vec)}
{-# INLINE unsafeDeleteMinBH #-}
modifyMinBH :: (PrimMonad m, U.Unbox a, Ord a) => BinaryHeap (PrimState m) a -> (a -> a) -> m ()
modifyMinBH bh@(BinaryHeap _ vec) f = do { UM.unsafeModify vec f 0; size <- getBinaryHeapSize bh; siftDown 0 (UM.unsafeTake size vec)}
{-# INLINE modifyMinBH #-}
deleteFindMinBH :: (PrimMonad m, U.Unbox a, Ord a) => BinaryHeap (PrimState m) a -> m (Maybe a)
deleteFindMinBH bh@(BinaryHeap _ vec) = do { size <- getBinaryHeapSize bh; if size > 0 then Just <$!> unsafeMinViewBH bh <* unsafeDeleteMinBH bh else return $! Nothing}
{-# INLINE deleteFindMinBH #-}
clearBH :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m ()
clearBH (BinaryHeap info _) = writeMutVar info 0
freezeInternalBinaryHeapBH :: (PrimMonad m, U.Unbox a) => BinaryHeap (PrimState m) a -> m (U.Vector a)
freezeInternalBinaryHeapBH bh@(BinaryHeap _ vec) = do { size <- getBinaryHeapSize bh; U.unsafeFreeze (UM.unsafeTake size vec)}
-------------------------------------------------------------------------------
-- Data.VecQueue
-------------------------------------------------------------------------------
data VecQueue s a = VecQueue{queueInfo :: !(UM.MVector s Int), queueData :: !(UM.MVector s a)}
getEnqueueCount :: (PrimMonad m) => VecQueue (PrimState m) a -> m Int
getEnqueueCount (VecQueue info _) = UM.unsafeRead info 1
{-# INLINE getEnqueueCount #-}
getDequeueCount :: (PrimMonad m) => VecQueue (PrimState m) a -> m Int
getDequeueCount (VecQueue info _) = UM.unsafeRead info 0
{-# INLINE getDequeueCount #-}
withCapacityQ :: (PrimMonad m, UM.Unbox a) => Int -> m (VecQueue (PrimState m) a)
withCapacityQ n = VecQueue <$> UM.replicate 2 0 <*> UM.unsafeNew n
newQueue :: (PrimMonad m, UM.Unbox a) => m (VecQueue (PrimState m) a)
newQueue = withCapacityQ (1024 * 1024)
freezeQueueData :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (U.Vector a)
freezeQueueData vq@(VecQueue info q) = do { len <- getEnqueueCount vq; U.unsafeFreeze $ UM.take len q}
lengthQ :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m Int
lengthQ vq = (-) <$> getEnqueueCount vq <*> getDequeueCount vq
dequeue :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (Maybe a)
dequeue vq@(VecQueue info q) = do { h <- getDequeueCount vq; t <- getEnqueueCount vq; if h < t then do { UM.unsafeWrite info 0 (h + 1); pure <$> UM.unsafeRead q h} else return Nothing}
{-# INLINE dequeue #-}
dequeueAll :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (U.Vector a)
dequeueAll vq@(VecQueue info q) = do { h <- getDequeueCount vq; t <- getEnqueueCount vq; U.unsafeFreeze $ UM.unsafeSlice h (t - h) q}
{-# INLINE dequeueAll #-}
enqueue :: (PrimMonad m, UM.Unbox a) => a -> VecQueue (PrimState m) a -> m ()
enqueue x vq@(VecQueue info q) = do { t <- getEnqueueCount vq; UM.unsafeWrite q t x; UM.unsafeWrite info 1 (t + 1)}
{-# INLINE enqueue #-}
