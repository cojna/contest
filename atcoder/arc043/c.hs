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
    n <- readLn @Int
    xs <- U.unfoldrN n (runParser int1) <$> C.getLine
    ys <- U.unfoldrN n (runParser int1) <$> C.getLine
    putStrLn.maybe "-1" (unwords.map show.U.toList) $ solve n xs ys

solve :: Int -> U.Vector Int -> U.Vector Int -> Maybe (U.Vector Int)
solve n xs0 ys0
    | odd invNum = Nothing
    | otherwise = Just . U.map (succ . U.unsafeIndex xs0)
        $ construct n (div invNum 2) ys
  where
    ixs = U.map snd . U.modify Intro.sort $ U.imap (flip(,)) xs0
    ys = U.backpermute ixs ys0
    invNum = inversionNumber ys

construct :: Int -> Int -> U.Vector Int -> U.Vector Int
construct n k xs = U.create $ do
    perm <- UM.unsafeNew n
    ft <- newSumFenwickTree @Int n
    fix (\loop !i !rest -> when (i >= 0) $ do
            inv <- sumTo ft (dict U.! i)
            addAt ft (dict U.! i) 1

            let pos = dict U.! i - inv
            let d = i - pos
            if d <= rest
            then do
                UM.write perm i i
                loop (i - 1) (rest - d)
            else do
                let xs' = U.filter (<=i) xs
                U.copy (UM.take (U.length xs') perm) xs'
                flip MS.mapM_ (stream pos (pos + rest)) $ \j -> do
                    UM.swap perm j (j + 1)
        ) (n - 1) k
    return perm
  where
    !dict = U.map snd . U.modify Intro.sort
        $ U.imap (flip (,)) xs

inversionNumber :: U.Vector Int -> Int
inversionNumber xs = runST $ do
    ft <- newSumFenwickTree (U.length xs)
    U.foldM'
        (\acc x -> do
            cnt <- sumFromTo ft x (U.length xs)
            addAt ft x 1
            return $! acc + cnt
        ) 0 xs

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
-- Data.FenwickTree
-------------------------------------------------------------------------------
newtype FenwickTree s a = FenwickTree{getFenwickTree :: UM.MVector s a}
newFenwickTree :: (U.Unbox a, Monoid a, PrimMonad m) => Int -> m (FenwickTree (PrimState m) a)
newFenwickTree n = FenwickTree <$> UM.replicate (n + 1) mempty
{-# INLINE newFenwickTree #-}
buildFenwickTree :: (U.Unbox a, Monoid a, PrimMonad m) => U.Vector a -> m (FenwickTree (PrimState m) a)
buildFenwickTree vec = do { let { n = U.length vec}; ft <- UM.unsafeNew (n + 1); UM.write ft 0 mempty; U.unsafeCopy (UM.tail ft) vec; flip fix 1 $ \ loop !i -> when (i <= n) $ do { let { j = i + (i .&. (-i))}; when (j <= n) $ do { fti <- UM.unsafeRead ft i; UM.unsafeModify ft (<> fti) j}; loop (i + 1)}; return $ FenwickTree ft}
{-# INLINE buildFenwickTree #-}
mappendTo :: (PrimMonad m, U.Unbox a, Monoid a) => FenwickTree (PrimState m) a -> Int -> m a
mappendTo (FenwickTree ft) = go mempty where { go !acc !i | i > 0 = do { xi <- UM.unsafeRead ft i; go (acc <> xi) (i - (i .&. (-i)))} | otherwise = return acc}
{-# INLINE mappendTo #-}
mappendAt :: (U.Unbox a, Semigroup a, PrimMonad m) => FenwickTree (PrimState m) a -> Int -> a -> m ()
mappendAt (FenwickTree ft) k v = flip fix (k + 1) $ \ loop !i -> do { when (i < n) $ do { UM.unsafeModify ft (<> v) i; loop $ i + (i .&. (-i))}} where { !n = UM.length ft}
{-# INLINE mappendAt #-}
type SumFenwickTree s a = FenwickTree s (Sum a)
newSumFenwickTree :: (Num a, U.Unbox a, PrimMonad m) => Int -> m (SumFenwickTree (PrimState m) a)
newSumFenwickTree = newFenwickTree
{-# INLINE newSumFenwickTree #-}
buildSumFenwickTree :: (Num a, U.Unbox a, PrimMonad m) => U.Vector a -> m (SumFenwickTree (PrimState m) a)
buildSumFenwickTree = buildFenwickTree . U.map coerce
{-# INLINE buildSumFenwickTree #-}
sumTo :: (Num a, U.Unbox a, PrimMonad m) => SumFenwickTree (PrimState m) a -> Int -> m a
sumTo ft k = coerce <$> mappendTo ft k
{-# INLINE sumTo #-}
sumFromTo :: (Num a, U.Unbox a, PrimMonad m) => SumFenwickTree (PrimState m) a -> Int -> Int -> m a
sumFromTo ft l r = (-) <$> sumTo ft r <*> sumTo ft l
{-# INLINE sumFromTo #-}
readSumFenwickTree :: (Num a, U.Unbox a, PrimMonad m) => SumFenwickTree (PrimState m) a -> Int -> m a
readSumFenwickTree ft i = sumFromTo ft i (i + 1)
{-# INLINE readSumFenwickTree #-}
writeSumFenwickTree :: (Num a, U.Unbox a, PrimMonad m) => SumFenwickTree (PrimState m) a -> Int -> a -> m ()
writeSumFenwickTree ft i x = readSumFenwickTree ft i >>= addAt ft i . (x -)
{-# INLINE writeSumFenwickTree #-}
addAt :: (U.Unbox a, Num a, PrimMonad m) => SumFenwickTree (PrimState m) a -> Int -> a -> m ()
addAt ft k x = mappendAt ft k (coerce x)
{-# INLINE addAt #-}
findMaxIndexLT :: (U.Unbox a, Num a, Ord a, PrimMonad m) => FenwickTree (PrimState m) a -> a -> m Int
findMaxIndexLT (FenwickTree ft) w0 | w0 <= 0 = return 0 | otherwise = go w0 highestOneBit 0 where { n = UM.length ft; highestOneBit = until (> n) (* 2) 1 `quot` 2; go !w !step !i | step == 0 = return i | otherwise = do { if i + step < n then do { u <- UM.unsafeRead ft (i + step); if u < w then go (w - u) (step `unsafeShiftR` 1) (i + step) else go w (step `unsafeShiftR` 1) i} else go w (step `unsafeShiftR` 1) i}}
{-# INLINE findMaxIndexLT #-}
