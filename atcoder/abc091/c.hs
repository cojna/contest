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
import           Data.Proxy
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
import           GHC.TypeLits
import qualified System.IO                     as IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    n <- readLn :: IO Int
    ps <- U.unfoldrN (2*n) (runParser $ (,) <$> int <*> int) <$> C.getContents
    print $ solve n $ U.splitAt n ps

isConnected :: (Int, Int) -> (Int, Int) -> Bool
isConnected (x0, y0) (x1, y1) = x0 < x1 && y0 < y1

solve :: Int -> (U.Vector (Int, Int), U.Vector (Int, Int)) -> Int
solve n (reds, blues) = maxFlow (2 + n * 2) src sink $ \builder -> do
    forM_  ((,) <$> rs <*> bs) $ \((i,r), (j,b)) -> when (isConnected r b) $ do
        addEdgeMFB builder (i, j, 1)
    rep n $ \i -> do
        addEdgeMFB builder (src, i, 1)
        addEdgeMFB builder (n+i, sink, 1)
  where
    numV = 2 * n + 2
    rs = zip [0..n-1] $ U.toList reds
    bs = zip [n..] $ U.toList blues
    src = 2 * n
    sink = src + 1

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
-- Data.Graph.MaxFlow
-------------------------------------------------------------------------------
nothing :: Int
nothing = -1
inf :: (Num a) => a
inf = 69540876599103
{-# INLINE inf #-}
type Vertex = Int
maxFlow :: (U.Unbox cap, Num cap, Ord cap) => Int -> Vertex -> Vertex -> (forall s . MaxFlowBuilder s cap -> ST s ()) -> cap
maxFlow numVertices src sink run = runST $ do { builder <- newMaxFlowBuilder numVertices; run builder; buildMaxFlow builder >>= runMaxFlow src sink}
data MaxFlow s cap = MaxFlow{numVerticesMF :: !Int, numEdgesMF :: !Int, offsetMF :: U.Vector Int, dstMF :: U.Vector Vertex, residualMF :: UM.MVector s cap, levelMF :: UM.MVector s Int, revEdgeMF :: U.Vector Int, iterMF :: UM.MVector s Int, queueMF :: VecQueue s Vertex}
runMaxFlow :: (U.Unbox cap, Num cap, Ord cap, PrimMonad m) => Vertex -> Vertex -> MaxFlow (PrimState m) cap -> m cap
runMaxFlow src sink mf@MaxFlow{..} = do { flip fix 0 $ \ loopBFS !flow -> do { UM.set levelMF nothing; clearVQ queueMF; bfsMF src mf; lsink <- UM.unsafeRead levelMF sink; if lsink == nothing then return flow else do { U.unsafeCopy iterMF offsetMF; flip fix flow $ \ loopDFS !f -> do { df <- dfsMF src sink inf mf; if df > 0 then loopDFS (f + df) else loopBFS f}}}}
bfsMF :: (Num cap, Ord cap, U.Unbox cap, PrimMonad m) => Vertex -> MaxFlow (PrimState m) cap -> m ()
bfsMF src MaxFlow{..} = do { UM.unsafeWrite levelMF src 0; enqueueVQ src queueMF; fix $ \ loop -> do { dequeueVQ queueMF >>= \case { Just v -> do { let { start = U.unsafeIndex offsetMF v}; let { end = U.unsafeIndex offsetMF (v + 1)}; U.forM_ (U.generate (end - start) (+ start)) $ \ e -> do { let { nv = U.unsafeIndex dstMF e}; res <- UM.unsafeRead residualMF e; lnv <- UM.unsafeRead levelMF nv; when (res > 0 && lnv == nothing) $ do { UM.unsafeRead levelMF v >>= UM.unsafeWrite levelMF nv . (+ 1); enqueueVQ nv queueMF}; loop}}; Nothing -> return ()}}}
{-# INLINE bfsMF #-}
dfsMF :: (U.Unbox cap, Num cap, Ord cap, PrimMonad m) => Vertex -> Vertex -> cap -> MaxFlow (PrimState m) cap -> m cap
dfsMF v0 sink flow0 MaxFlow{..} = dfs v0 flow0 return where { dfs !v !flow k | v == sink = k flow | otherwise = fix $ \ loop -> do { e <- UM.unsafeRead iterMF v; if e < U.unsafeIndex offsetMF (v + 1) then do { UM.unsafeWrite iterMF v (e + 1); let { nv = U.unsafeIndex dstMF e}; cap <- UM.unsafeRead residualMF e; lv <- UM.unsafeRead levelMF v; lnv <- UM.unsafeRead levelMF nv; if cap > 0 && lv < lnv then do { dfs nv (min flow cap) $ \ f -> do { if f > 0 then do { UM.unsafeModify residualMF (subtract f) e; UM.unsafeModify residualMF (+ f) (U.unsafeIndex revEdgeMF e); k f} else loop}} else loop} else k 0}}
{-# INLINE dfsMF #-}
data MaxFlowBuilder s cap = MaxFlowBuilder{numVerticesMFB :: !Int, inDegreeMFB :: UM.MVector s Int, edgesMFB :: VecQueue s (Vertex, Vertex, cap)}
newMaxFlowBuilder :: (U.Unbox cap, PrimMonad m) => Int -> m (MaxFlowBuilder (PrimState m) cap)
newMaxFlowBuilder n = MaxFlowBuilder n <$> UM.replicate n 0 <*> newVecQueue (1024 * 1024)
buildMaxFlow :: (Num cap, U.Unbox cap, PrimMonad m) => MaxFlowBuilder (PrimState m) cap -> m (MaxFlow (PrimState m) cap)
buildMaxFlow MaxFlowBuilder{..} = do { offsetMF <- U.scanl' (+) 0 <$> U.unsafeFreeze inDegreeMFB; let { numVerticesMF = numVerticesMFB}; let { numEdgesMF = U.last offsetMF}; moffset <- U.thaw offsetMF; mdstMF <- UM.replicate numEdgesMF nothing; mrevEdgeMF <- UM.replicate numEdgesMF nothing; residualMF <- UM.replicate numEdgesMF 0; edges <- freezeVecQueue edgesMFB; U.forM_ edges $ \ (src, dst, cap) -> do { srcOffset <- UM.unsafeRead moffset src; dstOffset <- UM.unsafeRead moffset dst; UM.unsafeModify moffset (+ 1) src; UM.unsafeModify moffset (+ 1) dst; UM.unsafeWrite mdstMF srcOffset dst; UM.unsafeWrite mdstMF dstOffset src; UM.unsafeWrite mrevEdgeMF srcOffset dstOffset; UM.unsafeWrite mrevEdgeMF dstOffset srcOffset; UM.unsafeWrite residualMF srcOffset cap}; dstMF <- U.unsafeFreeze mdstMF; levelMF <- UM.replicate numVerticesMF nothing; revEdgeMF <- U.unsafeFreeze mrevEdgeMF; iterMF <- UM.replicate numVerticesMF 0; U.unsafeCopy iterMF offsetMF; queueMF <- newVecQueue numVerticesMF; return MaxFlow{..}}
addEdgeMFB :: (U.Unbox cap, PrimMonad m) => MaxFlowBuilder (PrimState m) cap -> (Vertex, Vertex, cap) -> m ()
addEdgeMFB MaxFlowBuilder{..} (!src, !dst, !cap) = do { UM.unsafeModify inDegreeMFB (+ 1) src; UM.unsafeModify inDegreeMFB (+ 1) dst; enqueueVQ (src, dst, cap) edgesMFB}
{-# INLINE addEdgeMFB #-}
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
