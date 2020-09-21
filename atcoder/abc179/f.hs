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
    [n, q] <- map read.words <$> getLine
    qs <- U.unfoldrN q (runParser $ (,) <$> int <*> fmap (subtract 2) int) <$> C.getContents
    print $ solve n q qs

instance MonoidAction (Min Int) (Min Int) where
    appMonoid x y = x <> y

solve :: Int -> Int -> U.Vector (Int, Int) -> Int
solve n q qs = runST $ do
    seg1 <- buildSegTreeLazy @(Min Int) @(Min Int) . U.replicate n $ Min (n - 2)
    seg2 <- buildSegTreeLazy @(Min Int) @(Min Int) . U.replicate n $ Min (n - 2)
    res <- fmap U.sum . U.forM qs $ \case
        (1, x) -> do
            rx <- getMin <$> readSegTreeLazy seg1 x
            appFromTo seg2 0 (rx + 1) (Min x)
            return $! rx
        (2, x) -> do
            rx <- getMin <$> readSegTreeLazy seg2 x
            appFromTo seg1 0 (rx + 1) (Min x)
            return $! rx
        _ -> undefined
    return $ (n - 2) * (n - 2) - res

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
-- Data.SegTree.Lazy
-------------------------------------------------------------------------------
data SegTreeLazy s a f = SegTreeLazy (UM.MVector s a) (UM.MVector s f)
newSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, PrimMonad m) => Int -> m (SegTreeLazy (PrimState m) a f)
newSegTreeLazy n = SegTreeLazy <$> UM.replicate (2 * extendToPowerOfTwo n) mempty <*> UM.replicate (extendToPowerOfTwo n) mempty
buildSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, PrimMonad m) => U.Vector a -> m (SegTreeLazy (PrimState m) a f)
buildSegTreeLazy xs = do { tree <- UM.replicate (2 * n) mempty; lazy <- UM.replicate n mempty; U.unsafeCopy (UM.unsafeSlice n (U.length xs) tree) xs; let { seg = SegTreeLazy tree lazy}; flip MS.mapM_ (streamR 1 n) $ \ i -> do { updateSegTreeLazy seg i}; return seg} where { !n = extendToPowerOfTwo $ U.length xs}
class (Monoid f) => MonoidAction f a where { appMonoid :: f -> a -> a}
appAllAt :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m) => SegTreeLazy (PrimState m) a f -> Int -> f -> m ()
appAllAt (SegTreeLazy tree lazy) k f = do { tk <- UM.unsafeModify tree (appMonoid f) k; when (k < UM.length lazy) $ do { UM.unsafeModify lazy (mappend f) k}}
{-# INLINE appAllAt #-}
pushSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m) => SegTreeLazy (PrimState m) a f -> Int -> m ()
pushSegTreeLazy st@(SegTreeLazy tree lazy) k = do { fk <- UM.unsafeRead lazy k; appAllAt st (2 * k) fk; appAllAt st (2 * k + 1) fk; UM.unsafeWrite lazy k mempty}
{-# INLINE pushSegTreeLazy #-}
updateSegTreeLazy :: (Monoid a, U.Unbox a, PrimMonad m) => SegTreeLazy (PrimState m) a f -> Int -> m ()
updateSegTreeLazy (SegTreeLazy tree _) k = do { (<>) <$> UM.unsafeRead tree (2 * k) <*> UM.unsafeRead tree (2 * k + 1) >>= UM.unsafeWrite tree k}
{-# INLINE updateSegTreeLazy #-}
writeSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m) => SegTreeLazy (PrimState m) a f -> Int -> a -> m ()
writeSegTreeLazy st@(SegTreeLazy tree lazy) k0 v = do { let { !n = UM.length lazy; k = k0 + n; !h = 64 - countLeadingZeros n}; flip MS.mapM_ (streamR 1 h) $ \ i -> do { pushSegTreeLazy st (unsafeShiftR k i)}; UM.unsafeWrite tree k v; flip MS.mapM_ (stream 1 h) $ \ i -> do { updateSegTreeLazy st (unsafeShiftR k i)}}
{-# INLINE writeSegTreeLazy #-}
readSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m) => SegTreeLazy (PrimState m) a f -> Int -> m a
readSegTreeLazy st@(SegTreeLazy tree lazy) k0 = do { let { !n = UM.length lazy; k = k0 + n; !h = 64 - countLeadingZeros n}; flip MS.mapM_ (streamR 1 h) $ \ i -> do { pushSegTreeLazy st (unsafeShiftR k i)}; UM.unsafeRead tree k}
{-# INLINE readSegTreeLazy #-}
mappendFromTo :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m) => SegTreeLazy (PrimState m) a f -> Int -> Int -> m a
mappendFromTo st@(SegTreeLazy tree lazy) l0 r0 = do { let { !n = UM.length lazy; !l = l0 + n; !r = r0 + n; !h = 64 - countLeadingZeros n}; flip MS.mapM_ (streamR 1 h) $ \ i -> do { when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do { pushSegTreeLazy st (unsafeShiftR l i)}; when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do { pushSegTreeLazy st (unsafeShiftR r i)}}; let { calcL l acc | l .&. 1 == 1 = mappend acc <$> UM.unsafeRead tree l | otherwise = return acc; calcR r acc | r .&. 1 == 1 = flip mappend acc <$> UM.unsafeRead tree (r - 1) | otherwise = return acc}; fix (\ loop !accL !accR !l' !r' -> do { if l' < r' then do { !accL' <- calcL l' accL; !accR' <- calcR r' accR; loop accL' accR' (unsafeShiftRL (l' + l' .&. 1) 1) (unsafeShiftRL (r' - r' .&. 1) 1)} else return $! accL <> accR}) mempty mempty l r}
{-# INLINE mappendFromTo #-}
mappendAll :: (Monoid a, U.Unbox a, PrimMonad m) => SegTreeLazy (PrimState m) a f -> m a
mappendAll (SegTreeLazy tree _) = UM.unsafeRead tree 1
{-# INLINE mappendAll #-}
appAt :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m) => SegTreeLazy (PrimState m) a f -> Int -> f -> m ()
appAt st@(SegTreeLazy tree lazy) k0 f = do { let { !n = UM.length lazy; k = k0 + n; !h = 64 - countLeadingZeros n}; flip MS.mapM_ (streamR 1 h) $ \ i -> do { pushSegTreeLazy st (unsafeShiftR k i)}; UM.unsafeModify tree (appMonoid f) k; flip MS.mapM_ (stream 1 h) $ \ i -> do { updateSegTreeLazy st (unsafeShiftR k i)}}
{-# INLINE appAt #-}
appFromTo :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m) => SegTreeLazy (PrimState m) a f -> Int -> Int -> f -> m ()
appFromTo st@(SegTreeLazy tree lazy) l0 r0 f = when (l0 < r0) $ do { let { !n = UM.length lazy; !l = l0 + n; !r = r0 + n; !h = 64 - countLeadingZeros n}; flip MS.mapM_ (streamR 1 h) $ \ i -> do { when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do { pushSegTreeLazy st (unsafeShiftRL l i)}; when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do { pushSegTreeLazy st (unsafeShiftRL (r - 1) i)}}; fix (\ loop !l' !r' -> when (l' < r') $ do { when (l' .&. 1 == 1) $ do { appAllAt st l' f}; when (r' .&. 1 == 1) $ do { appAllAt st (r' - 1) f}; loop (unsafeShiftRL (l' + l' .&. 1) 1) (unsafeShiftRL (r' - r' .&. 1) 1)}) l r; flip MS.mapM_ (stream 1 h) $ \ i -> do { when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do { updateSegTreeLazy st (unsafeShiftRL l i)}; when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do { updateSegTreeLazy st (unsafeShiftRL (r - 1) i)}}}
{-# INLINE appFromTo #-}
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1 | otherwise = 1
