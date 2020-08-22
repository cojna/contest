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
    [sx,sy] <- map (subtract 1.read).words <$> getLine
    [gx,gy] <- map (subtract 1.read).words <$> getLine
    mat <- U.unfoldrN (h*w) (runParser char) . C.filter (not.isSpace) <$> C.getContents
    print . maybe (-1) id$ solve h w (sx,sy) (gx,gy) mat

solve :: Int -> Int -> (Int, Int) -> (Int, Int) -> U.Vector Char -> Maybe Int
solve h w (sx,sy) (gx,gy) mat = runST $ do
    dist <- UM.replicate (h*w) maxBound
    heap <- newMinBinaryHeap 400400
    UM.write dist start 0
    insertBH (0, start) heap
    fix $ \loop -> deleteFindTopBH heap >>= \case
        Just (dv,xy) -> do
          dv' <- UM.unsafeRead dist xy
          when (dv == dv') $ do
            let (x, y) = quotRem xy w
            neighbor4 x y $ \nx ny -> when (inGrid h w nx ny && U.unsafeIndex mat (ix nx ny) == '.') $ do
                dnxny <- UM.unsafeRead dist (ix nx ny)
                when (dv < dnxny) $ do
                    UM.unsafeWrite dist (ix nx ny) dv
                    insertBH (dv, ix nx ny) heap
            flip MS.mapM_ (stream (x-2) (x+3)) $ \nx -> do
                flip MS.mapM_ (stream (y-2) (y+3)) $ \ny -> do
                    let nxny = ix nx ny
                    when (inGrid h w nx ny && U.unsafeIndex mat nxny == '.' ) $ do
                        dnxny <- UM.unsafeRead dist nxny
                        when (dv + 1 < dnxny) $ do
                            UM.unsafeWrite dist nxny (dv + 1)
                            insertBH (dv+1, nxny) heap
          loop
        Nothing -> return ()
    dg <- UM.read dist goal
    if dg == maxBound
    then return Nothing
    else return $ Just dg
  where
    ix i j = i * w + j
    {-# INLINE ix #-}
    start = ix sx sy
    goal = ix gx gy


inGrid :: Int -> Int -> Int -> Int -> Bool
inGrid h w x y = 0 <= x && x < h && 0 <= y && y < w
{-# INLINE inGrid #-}

neighbor4 :: (Applicative f) => Int -> Int -> (Int -> Int -> f ()) -> f ()
neighbor4 x y f = f (x - 1) y *> f x (y - 1) *> f x (y + 1) *> f (x + 1) y
{-# INLINE neighbor4 #-}

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
