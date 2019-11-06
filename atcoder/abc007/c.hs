{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances         #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses   #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeFamilies, ViewPatterns #-}

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
import           Data.Functor.Identity
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
    [r, c] <- map read.words <$> getLine :: IO [Int]
    [sx, sy] <- map read.words <$> getLine :: IO [Int]
    [gx, gy] <- map read.words <$> getLine :: IO [Int]
    m <- U.unfoldrN (r * (c + 1)) C.uncons <$> C.getContents
    print $ solve (r, c) (sx, sy) (gx, gy) m

solve :: (Int, Int) -> (Int, Int) -> (Int, Int) -> U.Vector Char -> Int
solve (r, c) (sx, sy) (gx, gy) grid = runST $ do
    dist <- UM.replicate (h * w) (-1)
    que <- newVecQueue (h * w)
    UM.write dist start 0
    enqueueVQ start que
    fix $ \loop -> do
        dequeueVQ que >>= \case
            Just top -> do
                let (i, j) = unIx top
                neighbor4 i j $ \ni nj -> do
                    when (inGrid h w ni nj && U.unsafeIndex grid (ix ni nj) == '.') $ do
                        d' <- UM.unsafeRead dist (ix ni nj)
                        when (d' < 0) $ do
                            d <- UM.unsafeRead dist (ix i j)
                            UM.unsafeWrite dist (ix ni nj) (d + 1)
                            enqueueVQ (ix ni nj) que
                loop
            Nothing -> UM.read dist goal
  where
    h = r
    w = c + 1
    ix i j = i * w + j
    unIx ij = quotRem ij w
    start = ix (sx - 1) (sy - 1)
    goal = ix (gx - 1) (gy - 1)

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
-- Data.Graph.Grid
-------------------------------------------------------------------------------
inGrid :: Int -> Int -> Int -> Int -> Bool
inGrid h w x y = 0 <= x && x < h && 0 <= y && y < w
{-# INLINE inGrid #-}
neighbor4 :: (Applicative f) => Int -> Int -> (Int -> Int -> f ()) -> f ()
neighbor4 x y f = f (x - 1) y *> f x (y - 1) *> f x (y + 1) *> f (x + 1) y
{-# INLINE neighbor4 #-}
mkGrid :: C.ByteString -> U.Vector Char
mkGrid bs = U.unfoldrN (C.length bs) C.uncons bs
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
