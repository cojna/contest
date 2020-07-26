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
import qualified System.IO                         as IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    n <- readLn @Int
    xyus <- U.unfoldrN n (runParser $ (,,) <$> int <*> int <* char <*> char) <$> C.getContents
    putStrLn.maybe "SAFE" show $ solve n xyus

solve :: Int -> U.Vector (Int, Int, Char) -> Maybe Int
solve n xyus = fmap getMin $ mconcat
    [ solveUR us rs
    , solveUD us ds
    , solveUL us ls
    , solveRD rs ds
    , solveRL rs ls
    , solveDL ds ls
    ]
  where
    us = U.mapMaybe (\(x, y, c) -> if c == 'U' then Just (x, y) else Nothing) xyus
    rs = U.mapMaybe (\(x, y, c) -> if c == 'R' then Just (x, y) else Nothing) xyus
    ds = U.mapMaybe (\(x, y, c) -> if c == 'D' then Just (x, y) else Nothing) xyus
    ls = U.mapMaybe (\(x, y, c) -> if c == 'L' then Just (x, y) else Nothing) xyus

lim :: Int
lim = 200200

maybeMin :: Int -> Maybe (Min Int)
maybeMin x
    | x == maxBound = Nothing
    | otherwise = Just (Min x)

solveUR :: U.Vector (Int, Int) -> U.Vector (Int, Int) -> Maybe (Min Int)
solveUR us rs
    | U.null us || U.null rs = Nothing
    | otherwise = maybeMin . U.minimum $ U.map calc rs
  where
    !us' = U.modify Intro.sort
        $ U.map (\(x, y) -> (x + y, x)) us
    calc (rx, ry)
        | not (r < U.last us') || rxy /= uxy = maxBound
        | otherwise = (ux - rx) * 10
      where
        !rxy = rx + ry
        !r = (rxy, rx)
        (uxy, ux) = U.unsafeIndex us' . lowerBound 0 (U.length us' - 1) $ (r <) . U.unsafeIndex us'

solveUD :: U.Vector (Int, Int) -> U.Vector (Int, Int) -> Maybe (Min Int)
solveUD us ds = solveRL (U.map swap us) (U.map swap ds)

solveUL :: U.Vector (Int, Int) -> U.Vector (Int, Int) -> Maybe (Min Int)
solveUL us ls = solveUR (U.map (first negate) us) (U.map (first negate) ls)

solveRD :: U.Vector (Int, Int) -> U.Vector (Int, Int) -> Maybe (Min Int)
solveRD rs ds = solveUR (U.map (second negate) ds) (U.map (second negate) rs)

solveRL :: U.Vector (Int, Int) -> U.Vector (Int, Int) -> Maybe (Min Int)
solveRL rs ls
    | U.null ls || U.null rs = Nothing
    | otherwise = maybeMin . U.minimum $ U.map calc rs
  where
    !ls' = runST $ do
        mv <- U.unsafeThaw $ U.map swap ls
        Intro.sort mv
        U.unsafeFreeze mv
    calc (rx, ry)
        | not (r < U.last ls') || ry /= ly = maxBound
        | otherwise = (lx - rx) * 10 `quot` 2
      where
        !r = (ry, rx)
        (ly, lx) = U.unsafeIndex ls' . lowerBound 0 (U.length ls' - 1) $ (r<) . U.unsafeIndex ls'

solveDL :: U.Vector (Int, Int) -> U.Vector (Int, Int) -> Maybe (Min Int)
solveDL ds ls = solveUR (U.map (bimap negate negate) ds) (U.map (bimap negate negate) ls)


-- | assert (p high)
lowerBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
lowerBoundM low high p = go low high
  where
    go !low !high
        | high <= low = return high
        | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid)
      where
        mid = low + unsafeShiftRL (high - low) 1
{-# INLINE lowerBoundM #-}

-- | assert (p low)
upperBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
upperBoundM low high p = do
    flg <- p high
    if flg
    then return high
    else subtract 1 <$!> lowerBoundM low high (fmap not.p)
{-# INLINE upperBoundM #-}

-- | assert (p high)
lowerBound :: Int -> Int -> (Int -> Bool) -> Int
lowerBound low high p = runIdentity (lowerBoundM low high (return . p))
{-# INLINE lowerBound #-}

-- | assert (p low)
upperBound :: Int -> Int -> (Int -> Bool) -> Int
upperBound low high p = runIdentity (upperBoundM low high (return . p))
{-# INLINE upperBound #-}

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
