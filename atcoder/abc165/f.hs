{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, ImplicitParams #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, PatternSynonyms, RankNTypes    #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections           #-}
{-# LANGUAGE TypeFamilies, UnboxedTuples, ViewPatterns                     #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Cont
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

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    es <- U.unfoldrN (n - 1) (runParser $ (,) <$> int1 <*> int1) <$> C.getContents
    putStr.unlines.map show.U.toList $ solve n xs es

root :: Int
root = 0

solve :: Int -> U.Vector Int -> U.Vector (Int, Int) -> U.Vector Int
solve n xs es = U.create $ do
    lis <- UM.replicate (n + 1) 0
    runDFS gr n root IS.empty $ \dfs p v s -> do
        let !x = xs U.! v
        case IS.lookupGE x s of
            Just !y
                | x < y -> do
                    UM.read lis p >>= UM.write lis v
                    U.foldr' ((>=>) . dfs v) (\s' -> return $! IS.insert y $ IS.delete x s') (nexts p v)
                        $! IS.insert x $ IS.delete y s
                | otherwise -> do
                    UM.read lis p >>= UM.write lis v
                    U.foldr' ((>=>) . dfs v) return (nexts p v) s
            Nothing -> do
                UM.read lis p >>= UM.write lis v . (+ 1)
                U.foldr' ((>=>) . dfs v) (\s' -> return $! IS.delete x s') (nexts p v)
                    $! IS.insert x s
    return $ UM.init lis
  where
    !gr = buildUndirectedGraph n es

    nexts p v = U.filter (/= p) $ gr `adj` v

runDFS :: (Monad m) => SparseGraph () -> Vertex -> Vertex -> s
    -> ((Vertex -> Vertex -> s -> m s)
            -> Vertex -> Vertex -> s -> m s)
    -> m s
runDFS gr nothing root s0 = \dfs -> fix dfs nothing root s0
{-# INLINE runDFS #-}

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
-- Data.Graph.Sparse
-------------------------------------------------------------------------------
type Vertex = Int
type Edge = (Vertex, Vertex)
type EdgeWith w = (Vertex, Vertex, w)
type EdgeId = Int
data SparseGraph w = CSR{numVerticesCSR :: !Int, numEdgesCSR :: !Int, offsetCSR :: !(U.Vector Int), adjacentCSR :: !(U.Vector Vertex), edgeCtxCSR :: !(U.Vector w)}
data SparseGraphBuilder s w = SparseGraphBuilder{numVerticesSGB :: !Int, queueSGB :: VecQueue s (EdgeWith w), outDegSGB :: UM.MVector s Int}
buildSparseGraph :: (U.Unbox w) => Int -> (forall s . SparseGraphBuilder s w -> ST s ()) -> SparseGraph w
buildSparseGraph numVerticesCSR run = runST $ do { queueSGB <- newVecQueue (1024 * 1024); outDegSGB <- UM.replicate numVerticesCSR 0; run SparseGraphBuilder{numVerticesSGB = numVerticesCSR, ..}; numEdgesCSR <- lengthVQ queueSGB; offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDegSGB; moffset <- U.thaw offsetCSR; madj <- UM.unsafeNew numEdgesCSR; mectx <- UM.unsafeNew numEdgesCSR; edges <- freezeVecQueue queueSGB; U.forM_ edges $ \ (src, dst, w) -> do { pos <- UM.unsafeRead moffset src; UM.unsafeWrite moffset src (pos + 1); UM.unsafeWrite madj pos dst; UM.unsafeWrite mectx pos w}; adjacentCSR <- U.unsafeFreeze madj; edgeCtxCSR <- U.unsafeFreeze mectx; return CSR{..}}
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
