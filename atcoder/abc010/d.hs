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
import           Data.Ratio
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
    [n, g, e] <- map read.words <$> getLine :: IO [Int]
    ps <- U.unfoldrN g (runParser int) <$> C.getLine
    es <- U.unfoldrN e (runParser $ (,) <$> int <*> int) <$> C.getContents
    print $ solve n ps es

solve :: Int -> U.Vector Int -> U.Vector (Int, Int) -> Int
solve n ps es = runST $ do
    let source = 0
    let sink = n
    mfb <- newMaxFlowBuilder (n + 1)
    U.forM_ ps $ \p -> do
        addEdgeMFB p sink 1 mfb
    U.forM_ es $ \(a, b) -> do
        addEdgeMFB a b 1 mfb
        addEdgeMFB b a 1 mfb
    mf <- buildMaxFlow mfb
    runMaxFlow source sink mf

nothing :: Int
nothing = -1

inf :: (Num a) => a
inf = 0x3f3f3f3f3f3f
{-# INLINE inf #-}

type Vertex = Int

data MaxFlow s cap = MaxFlow
    { numVerticesMF :: !Int
    , numEdgesMF    :: !Int
    , offsetMF      :: U.Vector Int
    , dstMF         :: U.Vector Vertex
    , residualMF    :: UM.MVector s cap
    , levelMF       :: UM.MVector s Int
    , revEdgeMF     :: U.Vector Int
    , iterMF        :: UM.MVector s Int
    , queueMF       :: VecQueue s Vertex
    }

-- | Dinic O(V^2E)
runMaxFlow :: (U.Unbox cap, Num cap, Ord cap, PrimMonad m, Show cap)
    => Vertex -> Vertex -> MaxFlow (PrimState m) cap -> m cap
runMaxFlow src sink mf@MaxFlow{..} = do
    flip fix 0 $ \loopBFS !flow -> do
        UM.set levelMF nothing
        clearVQ queueMF
        bfsMF src mf
        lsink <- UM.unsafeRead levelMF sink
        if lsink == nothing
        then return flow
        else do
            U.unsafeCopy iterMF offsetMF
            flip fix flow $ \loopDFS !f -> do
                df <- dfsMF src sink inf mf
                if df > 0
                then loopDFS (f + df)
                else loopBFS f


bfsMF :: (Num cap, Ord cap, U.Unbox cap, PrimMonad m)
    => Vertex -> MaxFlow (PrimState m) cap -> m ()
bfsMF src MaxFlow{..} = do
    UM.unsafeWrite levelMF src 0
    enqueue src queueMF
    fix $ \loop -> do
        dequeue queueMF >>= \case
            Just v -> do
                let start = U.unsafeIndex offsetMF v
                let end = U.unsafeIndex offsetMF (v + 1)
                U.forM_ (U.generate (end - start) (+start)) $ \e -> do
                    let nv = U.unsafeIndex dstMF e
                    res <- UM.unsafeRead residualMF e
                    lnv <- UM.unsafeRead levelMF nv
                    when (res > 0 && lnv == nothing) $ do
                        UM.unsafeRead levelMF v >>= UM.unsafeWrite levelMF nv . (+1)
                        enqueue nv queueMF
                    loop
            Nothing -> return ()
{-# INLINE bfsMF #-}

dfsMF :: (U.Unbox cap, Num cap, Ord cap, PrimMonad m, Show cap)
    => Vertex -> Vertex -> cap -> MaxFlow (PrimState m) cap -> m cap
dfsMF v0 sink flow0 MaxFlow{..} = dfs v0 flow0 return
  where
    dfs !v !flow k
        | v == sink = k flow
        | otherwise = fix $ \loop -> do
            e <- UM.unsafeRead iterMF v
            if e < U.unsafeIndex offsetMF (v + 1)
            then do
                UM.unsafeWrite iterMF v (e + 1)
                let nv = U.unsafeIndex dstMF e
                cap <- UM.unsafeRead residualMF e
                lv <- UM.unsafeRead levelMF v
                lnv <- UM.unsafeRead levelMF nv
                if cap > 0 && lv < lnv
                then do
                    dfs nv (min flow cap) $ \f -> do
                        if f > 0
                        then do
                            UM.unsafeModify residualMF (subtract f) e
                            UM.unsafeModify residualMF (+f)
                                (U.unsafeIndex revEdgeMF e)
                            k f
                        else loop
                else loop
            else k 0
{-# INLINE dfsMF #-}

data MaxFlowBuilder s cap = MaxFlowBuilder
    { numVerticesMFB :: !Int
    , inDegreeMFB    :: UM.MVector s Int
    , edgesMFB       :: MutVar s [(Vertex, Vertex, cap)]
    }

newMaxFlowBuilder :: (PrimMonad m)
    => Int -> m (MaxFlowBuilder (PrimState m) cap)
newMaxFlowBuilder n = MaxFlowBuilder n
    <$> UM.replicate n 0
    <*> newMutVar []

buildMaxFlowBuilder :: (PrimMonad m)
    => Int -> [(Vertex, Vertex, cap)] -> m (MaxFlowBuilder (PrimState m) cap)
buildMaxFlowBuilder n edges = do
    mfb <- newMaxFlowBuilder n
    forM_ edges $ \(src, dst, cap) -> do
        addEdgeMFB src dst cap mfb
    return mfb

addEdgeMFB :: (PrimMonad m)
    => Vertex -> Vertex -> cap -> MaxFlowBuilder (PrimState m) cap -> m ()
addEdgeMFB !src !dst !cap MaxFlowBuilder{..} = do
    UM.unsafeModify inDegreeMFB (+1) src
    UM.unsafeModify inDegreeMFB (+1) dst
    modifyMutVar' edgesMFB ((src, dst, cap):)
{-# INLINE addEdgeMFB #-}

buildMaxFlow :: (Num cap, U.Unbox cap, PrimMonad m)
    => MaxFlowBuilder (PrimState m) cap -> m (MaxFlow (PrimState m) cap)
buildMaxFlow MaxFlowBuilder{..} = do
    offsetMF <- U.scanl' (+) 0 <$> U.unsafeFreeze inDegreeMFB
    let numVerticesMF = numVerticesMFB
    let numEdgesMF = U.last offsetMF

    moffset <- U.thaw offsetMF
    mdstMF <- UM.replicate numEdgesMF nothing
    mrevEdgeMF <- UM.replicate numEdgesMF nothing
    residualMF <- UM.replicate numEdgesMF 0

    edges <- readMutVar edgesMFB
    forM_ edges $ \(src, dst, cap) -> do
        srcOffset <- UM.unsafeRead moffset src
        dstOffset <- UM.unsafeRead moffset dst
        UM.unsafeModify moffset (+1) src
        UM.unsafeModify moffset (+1) dst
        UM.unsafeWrite mdstMF srcOffset dst
        UM.unsafeWrite mdstMF dstOffset src
        UM.unsafeWrite mrevEdgeMF srcOffset dstOffset
        UM.unsafeWrite mrevEdgeMF dstOffset srcOffset
        UM.unsafeWrite residualMF srcOffset cap

    dstMF <- U.unsafeFreeze mdstMF
    levelMF <- UM.replicate numVerticesMF nothing
    revEdgeMF <- U.unsafeFreeze mrevEdgeMF
    iterMF <- UM.replicate numVerticesMF 0
    U.unsafeCopy iterMF offsetMF
    queueMF <- newVecQueue numVerticesMF
    return MaxFlow{..}


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
newVecQueue :: (PrimMonad m, UM.Unbox a) => Int -> m (VecQueue (PrimState m) a)
newVecQueue n = VecQueue <$> UM.replicate 2 0 <*> UM.unsafeNew n
defaultVecQueueSize :: Int
defaultVecQueueSize = 1024 * 1024
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
clearVQ :: (UM.Unbox a, PrimMonad m) => VecQueue (PrimState m) a -> m ()
clearVQ (VecQueue info _) = do { UM.unsafeWrite info 0 0; UM.unsafeWrite info 1 0}
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
