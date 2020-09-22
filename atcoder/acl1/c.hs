{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, DerivingStrategies  #-}
{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, LambdaCase #-}
{-# LANGUAGE MagicHash, MultiParamTypeClasses, MultiWayIf           #-}
{-# LANGUAGE NumericUnderscores, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RankNTypes, RecordWildCards, ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeApplications    #-}
{-# LANGUAGE TypeFamilies, TypeInType, UnboxedTuples, ViewPatterns  #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Bool
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Char8             as C
import qualified Data.ByteString.Internal          as B
import qualified Data.ByteString.Unsafe            as B
import           Data.Char
import qualified Data.Foldable                     as F
import           Data.Function
import           Data.Functor.Identity
import qualified Data.IntMap.Strict                as IM
import qualified Data.IntSet                       as IS
import qualified Data.List                         as L
import qualified Data.Map.Strict                   as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                          as S
import           Data.Tuple
import qualified Data.Vector                       as V
import qualified Data.Vector.Algorithms.Intro      as Intro
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Fusion.Bundle.Monadic as MB
import           Data.Vector.Fusion.Util
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import qualified Data.Vector.Mutable               as VM
import qualified Data.Vector.Primitive             as P
import qualified Data.Vector.Primitive.Mutable     as PM
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM
import           Debug.Trace
import           Foreign                           hiding (void)
import           GHC.Exts
import           GHC.TypeLits
import           System.IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    [h, w] <- map read.words <$> getLine
    mat <- U.unfoldrN (h*w) (runParser char)
        . C.filter (not.isSpace) <$> C.getContents
    print $ solve h w mat

solve :: Int -> Int -> U.Vector Char -> Int
solve h w mat = (inf * k - ) . fst $ minCostFlow (sink + 1) source sink k $ \builder -> do
    rep h $ \x -> do
        rep w $ \y -> do
            when (mat U.! ix x y == 'o') $ do
                addEdgeMCFB builder source (ix x y) (x + y) 1
            when (x + 1 < h && mat U.! ix (x + 1) y /= '#') $ do
                addEdgeMCFB builder (ix x y) (ix (x + 1) y) 0 k
            when (y + 1 < w && mat U.! ix x (y + 1) /= '#') $ do
                addEdgeMCFB builder (ix x y) (ix x (y + 1)) 0 k
            addEdgeMCFB builder (ix x y) sink (inf - x - y) 1
  where
    !k = U.length $ U.elemIndices 'o' mat
    ix i j = i * w + j
    {-# INLINE ix #-}
    source = h * w
    sink = source + 1
    inf = 255


-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (stream 0 n)
{-# INLINE rep #-}
rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev !n = flip MS.mapM_ (streamR 0 n)
{-# INLINE rev #-}
stream :: (Monad m) => Int -> Int -> MS.Stream m Int
stream !l !r = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream #-}
streamR :: (Monad m) => Int -> Int -> MS.Stream m Int
streamR !l !r = MS.Stream step (r - 1) where { step x | x >= l = return $ MS.Yield x (x - 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] streamR #-}
stream' :: (Monad m) => Int -> Int -> Int -> MS.Stream m Int
stream' !l !r !d = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + d) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream' #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL :: Int -> Int -> Int
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL :: Int -> Int -> Int
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
skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}
lowerBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
lowerBoundM low high p = go low high where { go !low !high | high <= low = return high | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid) where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE lowerBoundM #-}
upperBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
upperBoundM low high p = do { flg <- p high; if flg then return high else subtract 1 <$!> lowerBoundM low high (fmap not . p)}
{-# INLINE upperBoundM #-}
lowerBound :: Int -> Int -> (Int -> Bool) -> Int
lowerBound low high p = runIdentity (lowerBoundM low high (return . p))
{-# INLINE lowerBound #-}
upperBound :: Int -> Int -> (Int -> Bool) -> Int
upperBound low high p = runIdentity (upperBoundM low high (return . p))
{-# INLINE upperBound #-}
-------------------------------------------------------------------------------
-- Data.Graph.MinCostFlow
-------------------------------------------------------------------------------
nothingMCF :: Int
nothingMCF = -1
type Vertex = Int
type Cost = Int
type Capacity = Int
minCostFlow :: Int -> Vertex -> Vertex -> Capacity -> (forall s . MinCostFlowBuilder s -> ST s ()) -> (Cost, Capacity)
minCostFlow numVertices src sink flow run = runST $ do { builder <- newMinCostFlowBuilder numVertices; run builder; buildMinCostFlow builder >>= runMinCostFlow src sink flow}
data MinCostFlow s = MinCostFlow{numVerticesMCF :: !Int, numEdgesMCF :: !Int, offsetMCF :: U.Vector Int, dstMCF :: U.Vector Vertex, costMCF :: U.Vector Cost, residualMCF :: UM.MVector s Capacity, potentialMCF :: UM.MVector s Cost, distMCF :: UM.MVector s Cost, heapMCF :: MinBinaryHeap s Word64, revEdgeMCF :: U.Vector Int, prevVertexMCF :: UM.MVector s Vertex, prevEdgeMCF :: UM.MVector s Int}
runMinCostFlow :: (PrimMonad m) => Vertex -> Vertex -> Capacity -> MinCostFlow (PrimState m) -> m (Cost, Capacity)
runMinCostFlow source sink flow mcf@MinCostFlow{..} = go 0 flow where { go !res !f | f == 0 = return (res, flow) | otherwise = do { canFlow <- dijkstraMCF source sink mcf; if canFlow then do { rep numVerticesMCF $ \ v -> do { dv <- UM.unsafeRead distMCF v; UM.unsafeModify potentialMCF (+ dv) v}; flowed <- updateResidualMCF sink f mcf; hsink <- UM.unsafeRead potentialMCF sink; go (hsink * flowed + res) (f - flowed)} else return (res, flow - f)}}
encodeMCF :: Cost -> Vertex -> Word64
encodeMCF cost v = unsafeCoerce $ unsafeShiftL cost 16 .|. v
{-# INLINE encodeMCF #-}
decodeMCF :: Word64 -> (Cost, Vertex)
decodeMCF costv = unsafeCoerce (cost, v) where { !cost = unsafeShiftR costv 16; !v = costv .&. 65535}
{-# INLINE decodeMCF #-}
dijkstraMCF :: (PrimMonad m) => Vertex -> Vertex -> MinCostFlow (PrimState m) -> m Bool
dijkstraMCF source sink MinCostFlow{..} = do { UM.set distMCF maxBound; UM.unsafeWrite distMCF source 0; clearBH heapMCF; insertBH (encodeMCF 0 source) heapMCF; fix $ \ loop -> do { deleteFindTopBH heapMCF >>= \case { Just cv -> do { let { (c, v) = decodeMCF cv}; dv <- UM.unsafeRead distMCF v; unless (c > dv) $ do { let { start = U.unsafeIndex offsetMCF v}; let { end = U.unsafeIndex offsetMCF (v + 1)}; U.forM_ (U.generate (end - start) (+ start)) $ \ e -> do { let { nv = U.unsafeIndex dstMCF e}; let { v2nv = U.unsafeIndex costMCF e}; cap <- UM.unsafeRead residualMCF e; hv <- UM.unsafeRead potentialMCF v; hnv <- UM.unsafeRead potentialMCF nv; old <- UM.unsafeRead distMCF nv; let { dnv = dv + v2nv + hv - hnv}; when (cap > 0 && dnv < old) $ do { UM.unsafeWrite distMCF nv dnv; UM.unsafeWrite prevVertexMCF nv v; UM.unsafeWrite prevEdgeMCF nv e; insertBH (encodeMCF dnv nv) heapMCF}}}; loop}; Nothing -> do { cost <- UM.unsafeRead distMCF sink; return $! cost < maxBound}}}}
{-# INLINE dijkstraMCF #-}
updateResidualMCF :: (PrimMonad m) => Vertex -> Capacity -> MinCostFlow (PrimState m) -> m Capacity
updateResidualMCF sink flow MinCostFlow{..} = go sink flow return where { go !v !f k = do { pv <- UM.unsafeRead prevVertexMCF v; if pv < 0 then k f else do { pv2v <- UM.unsafeRead prevEdgeMCF v; f' <- UM.unsafeRead residualMCF pv2v; go pv (min f f') $ \ nf -> do { UM.unsafeModify residualMCF (subtract nf) pv2v; UM.unsafeModify residualMCF (+ nf) (U.unsafeIndex revEdgeMCF pv2v); k nf}}}}
{-# INLINE updateResidualMCF #-}
data MinCostFlowBuilder s = MinCostFlowBuilder{numVerticesMCFB :: !Int, inDegreeMCFB :: UM.MVector s Int, edgesMCFB :: VecQueue s (Vertex, Vertex, Cost, Capacity)}
newMinCostFlowBuilder :: (PrimMonad m) => Int -> m (MinCostFlowBuilder (PrimState m))
newMinCostFlowBuilder n = MinCostFlowBuilder n <$> UM.replicate n 0 <*> newVecQueue (1024 * 1024)
addEdgeMCFB :: (PrimMonad m) => MinCostFlowBuilder (PrimState m) -> Vertex -> Vertex -> Cost -> Capacity -> m ()
addEdgeMCFB MinCostFlowBuilder{..} src dst cost capacity = assert (cost >= 0) $ do { UM.unsafeModify inDegreeMCFB (+ 1) src; UM.unsafeModify inDegreeMCFB (+ 1) dst; enqueueVQ (src, dst, cost, capacity) edgesMCFB}
buildMinCostFlow :: (PrimMonad m) => MinCostFlowBuilder (PrimState m) -> m (MinCostFlow (PrimState m))
buildMinCostFlow MinCostFlowBuilder{..} = do { offsetMCF <- U.scanl' (+) 0 <$> U.unsafeFreeze inDegreeMCFB; let { numVerticesMCF = numVerticesMCFB}; let { numEdgesMCF = U.last offsetMCF}; moffset <- U.thaw offsetMCF; mdstMCF <- UM.replicate numEdgesMCF nothingMCF; mcostMCF <- UM.replicate numEdgesMCF 0; mrevEdgeMCF <- UM.replicate numEdgesMCF nothingMCF; residualMCF <- UM.replicate numEdgesMCF 0; edges <- freezeVecQueue edgesMCFB; U.forM_ edges $ \ (src, dst, cost, capacity) -> do { srcOffset <- UM.unsafeRead moffset src; dstOffset <- UM.unsafeRead moffset dst; UM.unsafeModify moffset (+ 1) src; UM.unsafeModify moffset (+ 1) dst; UM.unsafeWrite mdstMCF srcOffset dst; UM.unsafeWrite mdstMCF dstOffset src; UM.unsafeWrite mcostMCF srcOffset cost; UM.unsafeWrite mcostMCF dstOffset (-cost); UM.unsafeWrite mrevEdgeMCF srcOffset dstOffset; UM.unsafeWrite mrevEdgeMCF dstOffset srcOffset; UM.unsafeWrite residualMCF srcOffset capacity}; dstMCF <- U.unsafeFreeze mdstMCF; costMCF <- U.unsafeFreeze mcostMCF; potentialMCF <- UM.replicate numVerticesMCF 0; distMCF <- UM.replicate numVerticesMCF 0; heapMCF <- newMinBinaryHeap numEdgesMCF; revEdgeMCF <- U.unsafeFreeze mrevEdgeMCF; prevVertexMCF <- UM.replicate numVerticesMCF nothingMCF; prevEdgeMCF <- UM.replicate numVerticesMCF nothingMCF; return MinCostFlow{..}}
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
