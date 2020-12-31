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
import           Data.Monoid                       hiding (First (..),
                                                    Last (..))
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                          as S
import           Data.Tuple
import qualified Data.Vector                       as V
import qualified Data.Vector.Algorithms.Intro      as Intro
import qualified Data.Vector.Fusion.Bundle.Monadic as MB
import qualified Data.Vector.Fusion.Stream.Monadic as MS
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

#define MOD 1000000007 /* 998244353 */

main :: IO ()
main = do
    [h,w] <- map read.words <$> getLine
    mat <- U.unfoldrN (h*w) (runParser char)
        . C.filter (not.isSpace) <$> C.getContents
    print . maybe (-1) id $ solve h w mat

solve :: Int -> Int -> U.Vector Char -> Maybe Int
solve h w mat
    | dist U.! goal >= inf = Nothing
    | otherwise = Just $ dist U.! goal
  where
    Just !start = U.elemIndex 'S' mat
    Just !goal = U.elemIndex 'G' mat
    inf = 0x3f3f3f3f3f3f3f3f
    ix x y = x * w + y
    {-# INLINE ix #-}
    !dist = bfs01CSR start gr

    !gr = buildSparseGraph (h*w+256) $ \builder -> do
        rep h $ \x -> do
            rep w $ \y -> do
                let xy = ix x y
                when (mat U.! xy /= '#') $ do
                    neighbor4' h w xy $ \nxy -> do
                        when (mat U.! nxy /= '#') $ do
                            addDirectedEdge builder (xy, nxy, 1)
        rep h $ \x -> do
            rep w $ \y -> do
                let xy = ix x y
                let cxy = mat U.! xy
                when ('a'<=cxy && cxy <='z')$do
                    addDirectedEdge builder (xy, h*w+ord cxy,0)
                    addDirectedEdge builder (h*w+ord cxy,xy,1)


neighbor4 :: (Applicative f) => Int -> Int -> Int -> Int -> (Int -> Int -> f ()) -> f ()
neighbor4 h w x y f
    =  when (x > 0) (f (x - 1) y)
    *> when (y > 0) (f x (y - 1))
    *> when (y < w - 1) (f x (y + 1))
    *> when (x < h - 1) (f (x + 1) y)
{-# INLINE neighbor4 #-}

neighbor4' :: (Applicative f) => Int -> Int -> Int -> (Int -> f ()) -> f ()
neighbor4' h w xy f
    =  when (x > 0) (f $ xy - w)
    *> when (y > 0) (f $ xy - 1)
    *> when (y < w - 1) (f $ xy + 1)
    *> when (x < h - 1) (f $ xy + w)
  where
    (!x, !y) = quotRem xy w
{-# INLINE neighbor4' #-}

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (stream 0 n)
{-# INLINE rep #-}
rep1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep1 n = flip MS.mapM_ (stream 1 (n + 1))
{-# INLINE rep1 #-}
rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev n = flip MS.mapM_ (streamR 0 n)
{-# INLINE rev #-}
rev1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev1 n = flip MS.mapM_ (streamR 1 (n + 1))
{-# INLINE rev1 #-}
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
binarySearchM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
binarySearchM low high p = go low high where { go !low !high | high <= low = return high | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid) where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE binarySearchM #-}
binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch low high p = runIdentity (binarySearchM low high (return . p))
{-# INLINE binarySearch #-}
radixSort :: U.Vector Int -> U.Vector Int
radixSort v = F.foldl' step v [0, 16, 32, 48] where { mask k x = unsafeShiftRL x k .&. 65535; step v k = U.create $ do { pos <- UM.unsafeNew 65537; UM.set pos 0; U.forM_ v $ \ x -> do { UM.unsafeModify pos (+ 1) (mask k x + 1)}; rep 65535 $ \ i -> do { fi <- UM.unsafeRead pos i; UM.unsafeModify pos (+ fi) (i + 1)}; res <- UM.unsafeNew $ U.length v; U.forM_ v $ \ x -> do { let { !masked = mask k x}; i <- UM.unsafeRead pos masked; UM.unsafeWrite pos masked $ i + 1; UM.unsafeWrite res i x}; return res}}
{-# INLINE radixSort #-}
encode32x2 :: Int -> Int -> Int
encode32x2 x y = unsafeShiftL x 32 .|. y
{-# INLINE encode32x2 #-}
decode32x2 :: Int -> (Int, Int)
decode32x2 xy = let { !x = unsafeShiftRL xy 32; !y = xy .&. 4294967295} in (x, y)
{-# INLINE decode32x2 #-}
-------------------------------------------------------------------------------
-- Data.Graph.Sparse
-------------------------------------------------------------------------------
type Vertex = Int
type Edge = (Vertex, Vertex)
type EdgeWith w = (Vertex, Vertex, w)
type EdgeId = Int
data SparseGraph w = CSR{numVerticesCSR :: !Int, numEdgesCSR :: !Int, offsetCSR :: !(U.Vector Int), adjacentCSR :: !(U.Vector Vertex), edgeCtxCSR :: !(U.Vector w)}
data SparseGraphBuilder s w = SparseGraphBuilder{numVerticesSGB :: !Int, queueSGB :: VecQueue s (EdgeWith w), outDegSGB :: UM.MVector s Int}
buildSparseGraph :: (U.Unbox w) => Int -> (forall s . SparseGraphBuilder s w -> ST s ()) -> SparseGraph w
buildSparseGraph numVerticesCSR run = runST $ do { queueSGB <- newVecQueue (6*2048*2048); outDegSGB <- UM.replicate numVerticesCSR 0; run SparseGraphBuilder{numVerticesSGB = numVerticesCSR, ..}; numEdgesCSR <- lengthVQ queueSGB; offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDegSGB; moffset <- U.thaw offsetCSR; madj <- UM.unsafeNew numEdgesCSR; mectx <- UM.unsafeNew numEdgesCSR; edges <- freezeVecQueue queueSGB; U.forM_ edges $ \ (src, dst, w) -> do { pos <- UM.unsafeRead moffset src; UM.unsafeWrite moffset src (pos + 1); UM.unsafeWrite madj pos dst; UM.unsafeWrite mectx pos w}; adjacentCSR <- U.unsafeFreeze madj; edgeCtxCSR <- U.unsafeFreeze mectx; return CSR{..}}
{-# INLINE buildSparseGraph #-}
addDirectedEdge :: (U.Unbox w, PrimMonad m) => SparseGraphBuilder (PrimState m) w -> EdgeWith w -> m ()
addDirectedEdge SparseGraphBuilder{..} (src, dst, w) = do { enqueueVQ (src, dst, w) queueSGB; UM.unsafeModify outDegSGB (+ 1) src}
{-# INLINE addDirectedEdge #-}
addUndirectedEdge :: (U.Unbox w, PrimMonad m) => SparseGraphBuilder (PrimState m) w -> EdgeWith w -> m ()
addUndirectedEdge SparseGraphBuilder{..} (src, dst, w) = do { enqueueVQ (src, dst, w) queueSGB; enqueueVQ (dst, src, w) queueSGB; UM.unsafeModify outDegSGB (+ 1) src; UM.unsafeModify outDegSGB (+ 1) dst}
{-# INLINE addUndirectedEdge #-}
addDirectedEdge_ :: (PrimMonad m) => SparseGraphBuilder (PrimState m) () -> Edge -> m ()
addDirectedEdge_ SparseGraphBuilder{..} (src, dst) = do { enqueueVQ (src, dst, ()) queueSGB; UM.unsafeModify outDegSGB (+ 1) src}
{-# INLINE addDirectedEdge_ #-}
addUndirectedEdge_ :: (PrimMonad m) => SparseGraphBuilder (PrimState m) () -> Edge -> m ()
addUndirectedEdge_ SparseGraphBuilder{..} (src, dst) = do { enqueueVQ (src, dst, ()) queueSGB; enqueueVQ (dst, src, ()) queueSGB; UM.unsafeModify outDegSGB (+ 1) src; UM.unsafeModify outDegSGB (+ 1) dst}
{-# INLINE addUndirectedEdge_ #-}
buildDirectedGraph :: Int -> U.Vector Edge -> SparseGraph ()
buildDirectedGraph numVerticesCSR edges = buildSparseGraph numVerticesCSR $ \ builder -> do { U.mapM_ (addDirectedEdge_ builder) edges}
buildUndirectedGraph :: Int -> U.Vector Edge -> SparseGraph ()
buildUndirectedGraph numVerticesCSR edges = buildSparseGraph numVerticesCSR $ \ builder -> do { U.mapM_ (addUndirectedEdge_ builder) edges}
buildDirectedGraphW :: (U.Unbox w) => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildDirectedGraphW numVerticesCSR edges = buildSparseGraph numVerticesCSR $ \ builder -> do { U.mapM_ (addDirectedEdge builder) edges}
buildUndirectedGraphW :: (U.Unbox w) => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildUndirectedGraphW numVerticesCSR edges = buildSparseGraph numVerticesCSR $ \ builder -> do { U.mapM_ (addUndirectedEdge builder) edges}
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
-- Data.Graph.Sparse.BFS01
-------------------------------------------------------------------------------
bfs01CSR :: Vertex -> SparseGraph Int -> U.Vector Int
bfs01CSR source gr@CSR{..} = U.create $ do { dist <- UM.replicate numVerticesCSR maxBound; deque <- newDeque numEdgesCSR; UM.write dist source 0; pushFront (0, source) deque; fix $ \ loop -> popFront deque >>= \case { Just (dv, v) -> do { dv' <- UM.unsafeRead dist v; when (dv == dv') $ do { U.forM_ (gr `adjW` v) $ \ (nv, w) -> do { dnv <- UM.unsafeRead dist nv; when (dv + w < dnv) $ do { UM.unsafeWrite dist nv (dv + w); if w == 0 then pushFront (dv, nv) deque else pushBack (dv + 1, nv) deque}}}; loop}; Nothing -> return ()}; return dist}
-------------------------------------------------------------------------------
-- Data.Deque
-------------------------------------------------------------------------------
data Deque s a = Deque{dequeVars :: !(UM.MVector s Int), getDeque :: !(UM.MVector s a)}
_dequeFrontPos :: Int
_dequeFrontPos = 0
_dequeBackPos :: Int
_dequeBackPos = 1
newDeque :: (PrimMonad m, U.Unbox a) => Int -> m (Deque (PrimState m) a)
newDeque n = Deque <$> UM.replicate 2 n <*> UM.unsafeNew (2 * n)
defaultDequeSize :: Int
defaultDequeSize = 1024 * 1024
type Queue s a = Deque s a
newQueue :: (PrimMonad m, U.Unbox a) => Int -> m (Deque (PrimState m) a)
newQueue n = Deque <$> UM.replicate 2 0 <*> UM.unsafeNew n
type Stack s a = Deque s a
newStack :: (PrimMonad m, U.Unbox a) => Int -> m (Deque (PrimState m) a)
newStack n = Deque <$> UM.replicate 2 0 <*> UM.unsafeNew n
lengthDeque :: (PrimMonad m, U.Unbox a) => Deque (PrimState m) a -> m Int
lengthDeque (Deque info _) = (-) <$> UM.unsafeRead info _dequeBackPos <*> UM.unsafeRead info _dequeFrontPos
{-# INLINE lengthDeque #-}
popFront :: (PrimMonad m, U.Unbox a) => Deque (PrimState m) a -> m (Maybe a)
popFront (Deque info v) = do { f <- UM.unsafeRead info _dequeFrontPos; b <- UM.unsafeRead info _dequeBackPos; if f < b then do { UM.unsafeWrite info _dequeFrontPos (f + 1); pure <$> UM.unsafeRead v f} else return Nothing}
{-# INLINE popFront #-}
popBack :: (PrimMonad m, U.Unbox a) => Deque (PrimState m) a -> m (Maybe a)
popBack (Deque info v) = do { f <- UM.unsafeRead info _dequeFrontPos; b <- UM.unsafeRead info _dequeBackPos; if f < b then do { UM.unsafeWrite info _dequeBackPos (b - 1); pure <$> UM.unsafeRead v b} else return Nothing}
{-# INLINE popBack #-}
pushFront :: (PrimMonad m, U.Unbox a) => a -> Deque (PrimState m) a -> m ()
pushFront x (Deque info v) = do { f <- UM.unsafeRead info _dequeFrontPos; UM.unsafeWrite v (f - 1) x; UM.unsafeWrite info _dequeFrontPos (f - 1)}
{-# INLINE pushFront #-}
pushBack :: (PrimMonad m, U.Unbox a) => a -> Deque (PrimState m) a -> m ()
pushBack x (Deque info v) = do { b <- UM.unsafeRead info _dequeBackPos; UM.unsafeWrite v b x; UM.unsafeWrite info _dequeBackPos (b + 1)}
{-# INLINE pushBack #-}
pushFronts :: (PrimMonad m, U.Unbox a) => U.Vector a -> Deque (PrimState m) a -> m ()
pushFronts vec (Deque info v) = do { let { n = U.length vec}; f <- UM.unsafeRead info _dequeFrontPos; UM.unsafeWrite info _dequeFrontPos (f - n); U.unsafeCopy (UM.unsafeSlice (f - n) n v) vec}
{-# INLINE pushFronts #-}
pushBacks :: (PrimMonad m, U.Unbox a) => U.Vector a -> Deque (PrimState m) a -> m ()
pushBacks vec (Deque info v) = do { let { n = U.length vec}; b <- UM.unsafeRead info _dequeBackPos; UM.unsafeWrite info _dequeBackPos (b + n); U.unsafeCopy (UM.unsafeSlice b n v) vec}
{-# INLINE pushBacks #-}
clearDeque :: (U.Unbox a, PrimMonad m) => Deque (PrimState m) a -> m ()
clearDeque (Deque info v) = do { let { o = UM.length v `quot` 2}; UM.unsafeWrite info _dequeFrontPos o; UM.unsafeWrite info _dequeBackPos o}
freezeDeque :: (PrimMonad m, U.Unbox a) => Deque (PrimState m) a -> m (U.Vector a)
freezeDeque (Deque info v) = do { f <- UM.unsafeRead info _dequeFrontPos; b <- UM.unsafeRead info _dequeBackPos; U.freeze $ UM.unsafeSlice f (b - f) v}