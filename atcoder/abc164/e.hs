{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, DerivingStrategies      #-}
{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ImplicitParams, KindSignatures #-}
{-# LANGUAGE LambdaCase, MagicHash, MultiParamTypeClasses, MultiWayIf   #-}
{-# LANGUAGE NumericUnderscores, OverloadedStrings, PatternSynonyms     #-}
{-# LANGUAGE RankNTypes, RecordWildCards, ScopedTypeVariables           #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeApplications        #-}
{-# LANGUAGE TypeFamilies, TypeInType, UnboxedTuples, ViewPatterns      #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Unsafe        as B
import           Data.Char
import qualified Data.Foldable                 as F
import           Data.Function
import           Data.Functor.Identity
import qualified Data.IntMap.Strict            as IM
import qualified Data.IntSet                   as IS
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                      as S
import           Data.Tuple
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as GM
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Primitive         as P
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector.Unboxed.Mutable   as UM
import           Debug.Trace
import           Foreign                       hiding (void)
import           GHC.Exts
import qualified System.IO                     as IO
import           Unsafe.Coerce

#define INF 0x3f3f3f3f3f3f3f3f


main :: IO ()
main = do
    [n, m, s] <- map read.words <$> getLine :: IO [Int]
    dat <- U.unfoldrN (4 * m + 2 * n) (runParser int) <$> C.getContents
    let (es, vs) = U.splitAt (4 * m) dat
    putStr.unlines.map show.U.toList $ solve n m s es vs

solve :: Int -> Int -> Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
solve n m s es0 vs0 = U.tail . U.generate n $ \i ->
    U.minimum $ U.slice (vid i 0) 2600 dist
  where
    !dist = dijkstraCSR (vid 0 (min 2500 s)) gr
    vid :: Int -> Int -> Int
    vid i c = 2600 * i + c
    cMax :: Int
    cMax = 2500

    gr = createCSR (2600 * n) $ \builder -> do
        rep m $ \i -> do
            let !u = U.unsafeIndex es0 (4 * i + 0) - 1
                !v = U.unsafeIndex es0 (4 * i + 1) - 1
                !a = U.unsafeIndex es0 (4 * i + 2)
                !b = U.unsafeIndex es0 (4 * i + 3)
            rep (cMax+1) $ \c -> do
                when (c - a >= 0) $ do
                    addDirectedEdge builder (vid u c) (vid v (c - a)) b
                    addDirectedEdge builder (vid v c) (vid u (c - a)) b
        rep n $ \i -> do
            let !c = U.unsafeIndex vs0 (2 * i + 0)
                !d = U.unsafeIndex vs0 (2 * i + 1)
            rep (cMax + 1) $ \coin -> do
                when (coin + c <= cMax) $
                    addDirectedEdge builder (vid i coin) (vid i (coin + c)) d

data SparseGraphBuilder s w = SparseGraphBuilder
    { numVerticesSGB :: !Int
    , queueSGB :: VecQueue s (EdgeWith w)
    , outDegSGB :: UM.MVector s Int
    }

createCSR :: (U.Unbox w)
    => Int -> (forall s.SparseGraphBuilder s w -> ST s ()) -> SparseGraph w
createCSR numVerticesCSR run = runST $ do
    queueSGB <- newVecQueue (1024 * 1024)
    outDegSGB <- UM.replicate numVerticesCSR 0
    run SparseGraphBuilder{numVerticesSGB = numVerticesCSR,..}
    numEdgesCSR <- lengthVQ queueSGB
    offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDegSGB
    moffset <- U.thaw offsetCSR
    madj <- UM.unsafeNew numEdgesCSR
    mectx <- UM.unsafeNew numEdgesCSR
    edges <- freezeVecQueue queueSGB
    U.forM_ edges $ \(src, dst, w) -> do
        pos <- UM.unsafeRead moffset src
        UM.unsafeWrite moffset src (pos + 1)
        UM.unsafeWrite madj pos dst
        UM.unsafeWrite mectx pos w
    adjacentCSR <- U.unsafeFreeze madj
    edgeCtxCSR <- U.unsafeFreeze mectx
    return CSR{..}
{-# INLINE createCSR #-}

addDirectedEdge :: (U.Unbox w, PrimMonad m)
    => SparseGraphBuilder (PrimState m) w -> Vertex -> Vertex -> w -> m ()
addDirectedEdge SparseGraphBuilder{..} src dst w = do
    enqueueVQ (src, dst, w) queueSGB
    UM.unsafeModify outDegSGB (+1) src
{-# INLINE addDirectedEdge #-}

addDirectedEdges :: (U.Unbox w, PrimMonad m)
    => SparseGraphBuilder (PrimState m) w -> U.Vector (EdgeWith w) -> m ()
addDirectedEdges SparseGraphBuilder{..} edges = do
    enqueuesVQ edges queueSGB
    U.mapM_ (UM.unsafeModify outDegSGB (+1))
        . (\(x, _, _) -> x)
        $ U.unzip3 edges
{-# INLINE addDirectedEdges #-}

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}
rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}
type Parser a = StateT C.ByteString Maybe a
runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}
int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}
int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}
char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}
byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}

-------------------------------------------------------------------------------
-- Data.Heap.Binary
-------------------------------------------------------------------------------
data BinaryHeap (f :: * -> *) s a = BinaryHeap{priorityBH :: a -> f a, intVarsBH :: !(UM.MVector s Int), internalVecBH :: !(UM.MVector s a)}
_sizeBH :: Int
_sizeBH = 0
{-# INLINE _sizeBH #-}
type MinBinaryHeap s a = BinaryHeap Identity s a
type MaxBinaryHeap s a = BinaryHeap Down s a
newBinaryHeap :: (U.Unbox a, PrimMonad m) => (a -> f a) -> Int -> m (BinaryHeap f (PrimState m) a)
newBinaryHeap prio n = BinaryHeap prio <$> UM.replicate 1 0 <*> UM.unsafeNew n
newMinBinaryHeap :: (U.Unbox a, PrimMonad m) => Int -> m (MinBinaryHeap (PrimState m) a)
newMinBinaryHeap = newBinaryHeap Identity
newMaxBinaryHeap :: (U.Unbox a, PrimMonad m) => Int -> m (MaxBinaryHeap (PrimState m) a)
newMaxBinaryHeap = newBinaryHeap Down
getBinaryHeapSize :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m Int
getBinaryHeapSize BinaryHeap{..} = UM.unsafeRead intVarsBH _sizeBH
{-# INLINE getBinaryHeapSize #-}
siftUpBy :: (U.Unbox a, PrimMonad m) => (a -> a -> Ordering) -> Int -> UM.MVector (PrimState m) a -> m ()
siftUpBy cmp k vec = do { x <- UM.unsafeRead vec k; flip fix k $ \ loop !i -> if i > 0 then do { let { parent = (i - 1) `unsafeShiftR` 1}; p <- UM.unsafeRead vec parent; case cmp p x of { GT -> UM.unsafeWrite vec i p >> loop parent; _ -> UM.unsafeWrite vec i x}} else UM.unsafeWrite vec 0 x}
{-# INLINE siftUpBy #-}
siftDownBy :: (U.Unbox a, PrimMonad m) => (a -> a -> Ordering) -> Int -> UM.MVector (PrimState m) a -> m ()
siftDownBy cmp k vec = do { x <- UM.unsafeRead vec k; let { !n = UM.length vec}; flip fix k $ \ loop !i -> do { let { l = unsafeShiftL i 1 .|. 1}; let { r = l + 1}; if n <= l then UM.unsafeWrite vec i x else do { vl <- UM.unsafeRead vec l; if r < n then do { vr <- UM.unsafeRead vec r; case cmp vr vl of { LT -> case cmp x vr of { GT -> UM.unsafeWrite vec i vr >> loop r; _ -> UM.unsafeWrite vec i x}; _ -> case cmp x vl of { GT -> UM.unsafeWrite vec i vl >> loop l; _ -> UM.unsafeWrite vec i x}}} else case cmp x vl of { GT -> UM.unsafeWrite vec i vl >> loop l; _ -> UM.unsafeWrite vec i x}}}}
{-# INLINE siftDownBy #-}
heapifyBy :: (U.Unbox a, PrimMonad m) => (a -> a -> Ordering) -> UM.MVector (PrimState m) a -> m ()
heapifyBy cmp vec = do { rev (UM.length vec `quot` 2) $ \ i -> do { siftDownBy cmp i vec}}
{-# INLINE heapifyBy #-}
class OrdVia f a where { compareVia :: (a -> f a) -> a -> a -> Ordering}
instance (Ord a) => OrdVia Identity a where { compareVia _ = coerce (compare :: Identity a -> Identity a -> Ordering); {-# INLINE compareVia #-}}
instance (Ord a) => OrdVia Down a where { compareVia _ = coerce (compare :: Down a -> Down a -> Ordering); {-# INLINE compareVia #-}}
buildBinaryHeapVia :: (OrdVia f a, U.Unbox a, PrimMonad m) => (a -> f a) -> U.Vector a -> m (BinaryHeap f (PrimState m) a)
buildBinaryHeapVia ~priorityBH vec = do { intVarsBH <- UM.replicate 1 $ U.length vec; internalVecBH <- U.thaw vec; heapifyBy (compareVia priorityBH) internalVecBH; return $! BinaryHeap{..}}
{-# INLINE buildBinaryHeapVia #-}
buildMinBinaryHeap :: (Ord a, U.Unbox a, PrimMonad m) => U.Vector a -> m (BinaryHeap Identity (PrimState m) a)
buildMinBinaryHeap = buildBinaryHeapVia Identity
{-# INLINE buildMinBinaryHeap #-}
buildMaxBinaryHeap :: (Ord a, U.Unbox a, PrimMonad m) => U.Vector a -> m (BinaryHeap Down (PrimState m) a)
buildMaxBinaryHeap = buildBinaryHeapVia Down
{-# INLINE buildMaxBinaryHeap #-}
unsafeViewBH :: (U.Unbox a, PrimMonad m) => BinaryHeap f (PrimState m) a -> m a
unsafeViewBH BinaryHeap{..} = UM.unsafeRead internalVecBH 0
{-# INLINE unsafeViewBH #-}
viewBH :: (U.Unbox a, PrimMonad m) => BinaryHeap f (PrimState m) a -> m (Maybe a)
viewBH bh = do { size <- getBinaryHeapSize bh; if size > 0 then Just <$!> unsafeViewBH bh else return $! Nothing}
{-# INLINE viewBH #-}
insertBH :: (OrdVia f a, U.Unbox a, PrimMonad m) => a -> BinaryHeap f (PrimState m) a -> m ()
insertBH x BinaryHeap{..} = do { size <- UM.unsafeRead intVarsBH _sizeBH; UM.unsafeWrite intVarsBH _sizeBH (size + 1); UM.unsafeWrite internalVecBH size x; siftUpBy (compareVia priorityBH) size internalVecBH}
{-# INLINE insertBH #-}
unsafeDeleteBH :: (OrdVia f a, U.Unbox a, PrimMonad m) => BinaryHeap f (PrimState m) a -> m ()
unsafeDeleteBH BinaryHeap{..} = do { size' <- subtract 1 <$!> UM.unsafeRead intVarsBH _sizeBH; UM.unsafeWrite intVarsBH _sizeBH size'; UM.unsafeSwap internalVecBH 0 size'; siftDownBy (compareVia priorityBH) 0 (UM.unsafeTake size' internalVecBH)}
{-# INLINE unsafeDeleteBH #-}
modifyTopBH :: (OrdVia f a, U.Unbox a, PrimMonad m) => (a -> a) -> BinaryHeap f (PrimState m) a -> m ()
modifyTopBH f BinaryHeap{..} = do { UM.unsafeModify internalVecBH f 0; size <- UM.unsafeRead intVarsBH _sizeBH; siftDownBy (compareVia priorityBH) 0 (UM.unsafeTake size internalVecBH)}
{-# INLINE modifyTopBH #-}
deleteFindTopBH :: (Ord a, U.Unbox a, PrimMonad m) => MinBinaryHeap (PrimState m) a -> m (Maybe a)
deleteFindTopBH bh = do { size <- getBinaryHeapSize bh; if size > 0 then do { !top <- unsafeViewBH bh <* unsafeDeleteBH bh; return $ Just top} else return Nothing}
{-# INLINE deleteFindTopBH #-}
clearBH :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m ()
clearBH BinaryHeap{..} = UM.unsafeWrite intVarsBH 0 0
freezeInternalVecBH :: (U.Unbox a, PrimMonad m) => BinaryHeap f (PrimState m) a -> m (U.Vector a)
freezeInternalVecBH BinaryHeap{..} = do { size <- UM.unsafeRead intVarsBH _sizeBH; U.unsafeFreeze (UM.unsafeTake size internalVecBH)}
-------------------------------------------------------------------------------
-- Data.Graph.Sparse
-------------------------------------------------------------------------------
type Vertex = Int
type Edge = (Vertex, Vertex)
type EdgeWith w = (Vertex, Vertex, w)
type EdgeId = Int
data SparseGraph w = CSR{numVerticesCSR :: !Int, numEdgesCSR :: !Int, offsetCSR :: !(U.Vector Int), adjacentCSR :: !(U.Vector Vertex), edgeCtxCSR :: !(U.Vector w)}
buildDirectedGraph :: Int -> U.Vector Edge -> SparseGraph ()
buildDirectedGraph numVerticesCSR edges = runST $ do { let { numEdgesCSR = U.length edges}; let { offsetCSR = U.scanl' (+) 0 . U.unsafeAccumulate (+) (U.replicate numVerticesCSR 0) . U.map (flip (,) 1) . fst $ U.unzip edges}; moffset <- U.thaw offsetCSR; madj <- UM.unsafeNew numEdgesCSR; U.forM_ edges $ \ (src, dst) -> do { pos <- UM.unsafeRead moffset src; UM.unsafeWrite moffset src (pos + 1); UM.unsafeWrite madj pos dst}; adjacentCSR <- U.unsafeFreeze madj; return CSR{edgeCtxCSR = U.replicate numEdgesCSR (), ..}}
buildUndirectedGraph :: Int -> U.Vector Edge -> SparseGraph ()
buildUndirectedGraph numVerticesCSR edges = runST $ do { let { numEdgesCSR = 2 * U.length edges}; outDeg <- UM.replicate numVerticesCSR (0 :: Int); U.forM_ edges $ \ (x, y) -> do { UM.unsafeModify outDeg (+ 1) x; UM.unsafeModify outDeg (+ 1) y}; offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg; moffset <- U.thaw offsetCSR; madj <- UM.unsafeNew numEdgesCSR; U.forM_ edges $ \ (x, y) -> do { posX <- UM.unsafeRead moffset x; posY <- UM.unsafeRead moffset y; UM.unsafeWrite moffset x (posX + 1); UM.unsafeWrite moffset y (posY + 1); UM.unsafeWrite madj posX y; UM.unsafeWrite madj posY x}; adjacentCSR <- U.unsafeFreeze madj; return CSR{edgeCtxCSR = U.replicate numEdgesCSR (), ..}}
buildDirectedGraphW :: (U.Unbox w) => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildDirectedGraphW numVerticesCSR edges = runST $ do { let { numEdgesCSR = U.length edges}; let { offsetCSR = U.scanl' (+) 0 . U.unsafeAccumulate (+) (U.replicate numVerticesCSR 0) . U.map (flip (,) 1) . (\ (x, _, _) -> x) $ U.unzip3 edges}; moffset <- U.thaw offsetCSR; madj <- UM.unsafeNew numEdgesCSR; mectx <- UM.unsafeNew numEdgesCSR; U.forM_ edges $ \ (src, dst, w) -> do { pos <- UM.unsafeRead moffset src; UM.unsafeWrite moffset src (pos + 1); UM.unsafeWrite madj pos dst; UM.unsafeWrite mectx pos w}; adjacentCSR <- U.unsafeFreeze madj; edgeCtxCSR <- U.unsafeFreeze mectx; return CSR{..}}
buildUndirectedGraphW :: (U.Unbox w) => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildUndirectedGraphW numVerticesCSR edges = runST $ do { let { numEdgesCSR = 2 * U.length edges}; outDeg <- UM.replicate numVerticesCSR (0 :: Int); U.forM_ edges $ \ (x, y, _) -> do { UM.unsafeModify outDeg (+ 1) x; UM.unsafeModify outDeg (+ 1) y}; offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg; moffset <- U.thaw offsetCSR; madj <- UM.unsafeNew numEdgesCSR; mectx <- UM.unsafeNew numEdgesCSR; U.forM_ edges $ \ (x, y, w) -> do { posX <- UM.unsafeRead moffset x; posY <- UM.unsafeRead moffset y; UM.unsafeWrite moffset x (posX + 1); UM.unsafeWrite moffset y (posY + 1); UM.unsafeWrite madj posX y; UM.unsafeWrite madj posY x; UM.unsafeWrite mectx posX w; UM.unsafeWrite mectx posY w}; adjacentCSR <- U.unsafeFreeze madj; edgeCtxCSR <- U.unsafeFreeze mectx; return CSR{..}}
adj :: SparseGraph w -> Vertex -> U.Vector Vertex
adj CSR{..} v = U.unsafeSlice o (o' - o) adjacentCSR where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE adj #-}
iadj :: SparseGraph w -> Vertex -> U.Vector (EdgeId, Vertex)
iadj CSR{..} v = U.imap ((,) . (+ o)) $ U.unsafeSlice o (o' - o) adjacentCSR where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE iadj #-}
adjW :: (U.Unbox w) => SparseGraph w -> Vertex -> U.Vector (Vertex, w)
adjW CSR{..} v = U.zip (U.unsafeSlice o (o' - o) adjacentCSR) (U.unsafeSlice o (o' - o) edgeCtxCSR) where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE adjW #-}
iadjW :: (U.Unbox w) => SparseGraph w -> Vertex -> U.Vector (EdgeId, Vertex, w)
iadjW CSR{..} v = U.izipWith (\ i u w -> (i + o, u, w)) (U.unsafeSlice o (o' - o) adjacentCSR) (U.unsafeSlice o (o' - o) edgeCtxCSR) where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE iadjW #-}
outEdges :: SparseGraph w -> Vertex -> U.Vector EdgeId
outEdges CSR{..} v = U.generate (o' - o) (+ o) where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE outEdges #-}
outDegree :: SparseGraph w -> Vertex -> Int
outDegree CSR{..} v = U.unsafeIndex offsetCSR (v + 1) - U.unsafeIndex offsetCSR v
{-# INLINE outDegree #-}
outDegrees :: SparseGraph w -> U.Vector Int
outDegrees CSR{..} = U.zipWith (-) offsetCSR $ U.tail offsetCSR
{-# INLINE outDegrees #-}
-------------------------------------------------------------------------------
-- Data.Graph.Sparse.Dijkstra
-------------------------------------------------------------------------------
dijkstraCSR :: (U.Unbox w, Num w, Ord w) => Vertex -> SparseGraph w -> U.Vector w
dijkstraCSR source gr@CSR{..} = U.create $ do { dist <- UM.replicate numVerticesCSR INF; heap <- newMinBinaryHeap numEdgesCSR; UM.write dist source 0; insertBH (0, source) heap; fix $ \ loop -> do { deleteFindTopBH heap >>= \case { Just (d, v) -> do { dv <- UM.unsafeRead dist v; when (dv == d) $ do { U.forM_ (gr `adjW` v) $ \ (v', w') -> do { dv' <- UM.unsafeRead dist v'; when (dv + w' < dv') $ do { UM.unsafeWrite dist v' $ dv + w'; insertBH (dv + w', v') heap}}}; loop}; Nothing -> return ()}}; return dist}
-------------------------------------------------------------------------------
-- Data.VecQueue
-------------------------------------------------------------------------------
data VecQueue s a = VecQueue{intVarsVQ :: !(UM.MVector s Int), internalVecQueue :: !(UM.MVector s a)}
_dequeueCount :: Int
_dequeueCount = 0
{-# INLINE _dequeueCount #-}
_enqueueCount :: Int
_enqueueCount = 1
{-# INLINE _enqueueCount #-}
newVecQueue :: (PrimMonad m, UM.Unbox a) => Int -> m (VecQueue (PrimState m) a)
newVecQueue n = VecQueue <$> UM.replicate 2 0 <*> UM.unsafeNew n
defaultVecQueueSize :: Int
defaultVecQueueSize = 1024 * 1024
lengthVQ :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m Int
lengthVQ (VecQueue info _) = (-) <$> UM.unsafeRead info _enqueueCount <*> UM.unsafeRead info _dequeueCount
{-# INLINE lengthVQ #-}
dequeueVQ :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (Maybe a)
dequeueVQ (VecQueue info q) = do { f <- UM.unsafeRead info _dequeueCount; r <- UM.unsafeRead info _enqueueCount; if f < r then do { UM.unsafeWrite info _dequeueCount (f + 1); pure <$> UM.unsafeRead q f} else return Nothing}
{-# INLINE dequeueVQ #-}
enqueueVQ :: (PrimMonad m, UM.Unbox a) => a -> VecQueue (PrimState m) a -> m ()
enqueueVQ x (VecQueue info q) = do { r <- UM.unsafeRead info _enqueueCount; UM.unsafeWrite q r x; UM.unsafeWrite info _enqueueCount (r + 1)}
{-# INLINE enqueueVQ #-}
enqueuesVQ :: (PrimMonad m, UM.Unbox a) => U.Vector a -> VecQueue (PrimState m) a -> m ()
enqueuesVQ vec (VecQueue info q) = do { r <- UM.unsafeRead info _enqueueCount; UM.unsafeWrite info _enqueueCount (r + U.length vec); U.unsafeCopy (UM.unsafeSlice r (U.length vec) q) vec}
{-# INLINE enqueuesVQ #-}
clearVQ :: (UM.Unbox a, PrimMonad m) => VecQueue (PrimState m) a -> m ()
clearVQ (VecQueue info _) = do { UM.unsafeWrite info _dequeueCount 0; UM.unsafeWrite info _enqueueCount 0}
freezeVecQueue :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (U.Vector a)
freezeVecQueue (VecQueue info q) = do { f <- UM.unsafeRead info _dequeueCount; r <- UM.unsafeRead info _enqueueCount; U.unsafeFreeze $ UM.unsafeSlice f (r - f) q}
