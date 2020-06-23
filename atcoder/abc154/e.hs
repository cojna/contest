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
    cs <- map digitToInt <$> getLine
    k <- readLn
    print $ solve k cs

data DP = DP
    !Int !Int !Int !Int !Int
    deriving (Show)

extract :: Int -> DP -> Int
extract 1 (DP cnt _ s1 _ _) = s1 + bool 0 1 (cnt == 1)
extract 2 (DP cnt _ _ s2 _) = s2 + bool 0 1 (cnt == 2)
extract 3 (DP cnt _ _ _ s3) = s3 + bool 0 1 (cnt == 3)

solve :: Int -> [Int] -> Int
solve k (d:ds) = extract k $ F.foldl' step (DP 1 1 (d - 1) 0 0) ds
  where
    step (DP cnt s0 s1 s2 s3) 0 = DP cnt s0 s1' s2' s3'
        where
          s1' = s0 * 9 + s1
          s2' = s1 * 9 + s2
          s3' = s2 * 9 + s3

    step (DP cnt s0 s1 s2 s3) d = DP (cnt + 1) s0 s1' s2' s3'
      where
        s1' | cnt == 1 = 1 + s0 * 9 + s1
            | otherwise = s0 * 9 + s1
        s2' | cnt == 1 = d - 1 + s1 * 9 + s2
            | cnt == 2 = 1 + s1 * 9 + s2
            | otherwise = s1 * 9 + s2
        s3' | cnt == 2 = d - 1 + s2 * 9 + s3
            | cnt == 3 = 1 + s2 * 9 + s3
            | otherwise = s2 * 9 + s3

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

