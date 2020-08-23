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
    deque <- newDeque (2 * 1024 * 1024)
    UM.write dist start 0
    pushBack (0, start) deque
    fix $ \loop -> popFront deque >>= \case
        Just (dv, xy) -> do
          dv' <- UM.unsafeRead dist xy
          when (dv == dv') $ do
            let (x, y) = quotRem xy w
            neighbor4 h w x y $ \nx ny -> when (U.unsafeIndex mat (ix nx ny) == '.') $ do
                dnxny <- UM.unsafeRead dist (ix nx ny)
                when (dv < dnxny) $ do
                    UM.unsafeWrite dist (ix nx ny) dv
                    pushFront (dv, ix nx ny) deque
            flip MS.mapM_ (stream (max 0 $ x-2) (min h $ x+3)) $ \nx -> do
                flip MS.mapM_ (stream (max 0 $ y-2) (min w $ y+3)) $ \ny -> do
                    let nxny = ix nx ny
                    when (U.unsafeIndex mat nxny == '.' ) $ do
                        dnxny <- UM.unsafeRead dist nxny
                        when (dv + 1 < dnxny) $ do
                            UM.unsafeWrite dist nxny (dv + 1)
                            pushBack (dv + 1, nxny) deque
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

neighbor4 :: (Applicative f) => Int -> Int -> Int -> Int -> (Int -> Int -> f ()) -> f ()
neighbor4 h w x y f
    =  when (x > 0) (f (x - 1) y)
    *> when (y > 0) (f x (y - 1))
    *> when (y < w - 1) (f x (y + 1))
    *> when (x < h - 1) (f (x + 1) y)
{-# INLINE neighbor4 #-}

data Deque s a = Deque
    { dequeVars :: !(UM.MVector s Int)
    , getDeque  :: !(UM.MVector s a)
    }

_dequeFrontPos :: Int
_dequeFrontPos = 0

_dequeBackPos :: Int
_dequeBackPos = 1

newDeque :: (PrimMonad m, UM.Unbox a) => Int -> m (Deque (PrimState m) a)
newDeque n = Deque <$> UM.replicate 2 n <*> UM.unsafeNew (2 * n)

defaultDequeSize :: Int
defaultDequeSize = 1024 * 1024

lengthDeque :: (PrimMonad m, UM.Unbox a) => Deque (PrimState m) a -> m Int
lengthDeque (Deque info _)
    = (-) <$> UM.unsafeRead info _dequeBackPos
        <*> UM.unsafeRead info _dequeFrontPos
{-# INLINE lengthDeque #-}

popFront :: (PrimMonad m, UM.Unbox a) => Deque (PrimState m) a -> m (Maybe a)
popFront (Deque info v) = do
    f <- UM.unsafeRead info _dequeFrontPos
    b <- UM.unsafeRead info _dequeBackPos
    if f < b
    then do
        UM.unsafeWrite info _dequeFrontPos (f + 1)
        pure <$> UM.unsafeRead v f
    else return Nothing
{-# INLINE popFront #-}

popBack :: (PrimMonad m, UM.Unbox a) => Deque(PrimState m) a -> m (Maybe a)
popBack (Deque info v) = do
    f <- UM.unsafeRead info _dequeFrontPos
    b <- UM.unsafeRead info _dequeBackPos
    if f < b
    then do
        UM.unsafeWrite info _dequeBackPos (b - 1)
        pure <$> UM.unsafeRead v b
    else return Nothing
{-# INLINE popBack #-}

pushFront :: (PrimMonad m, UM.Unbox a) => a -> Deque(PrimState m) a -> m ()
pushFront x (Deque info v) = do
    f <- UM.unsafeRead info _dequeFrontPos
    UM.unsafeWrite v (f - 1) x
    UM.unsafeWrite info _dequeFrontPos (f - 1)
{-# INLINE pushFront #-}

pushBack :: (PrimMonad m, UM.Unbox a) => a -> Deque(PrimState m) a -> m ()
pushBack x (Deque info v) = do
    b <- UM.unsafeRead info _dequeBackPos
    UM.unsafeWrite v b x
    UM.unsafeWrite info _dequeBackPos (b + 1)
{-# INLINE pushBack #-}

pushFronts :: (PrimMonad m, UM.Unbox a)
    => U.Vector a -> Deque (PrimState m) a -> m ()
pushFronts vec (Deque info v) = do
    let n = U.length vec
    f <- UM.unsafeRead info _dequeFrontPos
    UM.unsafeWrite info _dequeFrontPos (f - n)
    U.unsafeCopy (UM.unsafeSlice (f - n) n v) vec
{-# INLINE pushFronts #-}

pushBacks :: (PrimMonad m, UM.Unbox a)
    => U.Vector a -> Deque (PrimState m) a -> m ()
pushBacks vec (Deque info v) = do
    let n = U.length vec
    b <- UM.unsafeRead info _dequeBackPos
    UM.unsafeWrite info _dequeBackPos (b + n)
    U.unsafeCopy (UM.unsafeSlice b n v) vec
{-# INLINE pushBacks #-}

clearDeque :: (UM.Unbox a, PrimMonad m) => Deque (PrimState m) a -> m ()
clearDeque (Deque info v) = do
    let o = UM.length v `quot` 2
    UM.unsafeWrite info _dequeFrontPos o
    UM.unsafeWrite info _dequeBackPos o

freezeDeque
    :: (PrimMonad m, UM.Unbox a)
    => Deque (PrimState m) a -> m (U.Vector a)
freezeDeque (Deque info v) = do
    f <- UM.unsafeRead info _dequeFrontPos
    b <- UM.unsafeRead info _dequeBackPos
    U.freeze $ UM.unsafeSlice f (b - f) v


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
