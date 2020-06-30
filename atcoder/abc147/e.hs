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
    [h, w] <- map read.words <$> getLine :: IO [Int]
    (a, b)<- U.splitAt (h*w).U.unfoldrN (2 * h * w) (runParser int)
        <$> C.getContents
    print $ solve h w $ U.zipWith (-) a b

ix :: Int -> Int -> Int -> Int
ix x y z = unsafeShiftL x 21 .|. unsafeShiftL y 14 .|. z
{-# INLINE ix #-}

lim :: Int
lim = 0x4000

solve :: Int -> Int -> U.Vector Int -> Int
solve h w mat = runST $ do
    dp <- UM.replicate (shiftL 1 28) False
    UM.write dp (ix 0 1 0) True
    UM.write dp (ix 1 0 0) True
    U.forM_ (U.generate h (+1)) $ \i -> do
        U.forM_ (U.generate w (+1)) $ \j -> do
            let !mij = abs $ U.unsafeIndex mat ((i - 1) * w + j - 1)
            rep lim $ \k -> do
                flg <- (||)
                    <$> UM.unsafeRead dp (ix (i-1) j (abs $ k - mij))
                    <*> UM.unsafeRead dp (ix i (j-1) (abs $ k - mij))
                when flg $ do
                    UM.unsafeWrite dp (ix i j k) True
                when (k + mij < lim) $ do
                    flg' <- (||)
                        <$> UM.unsafeRead dp (ix (i-1) j (k + mij))
                        <*> UM.unsafeRead dp (ix i (j-1) (k + mij))
                    when flg' $ do
                        UM.unsafeWrite dp (ix i j k) True
    fst . U.head . U.dropWhile (not.snd) . U.indexed
        <$> U.unsafeFreeze (UM.slice (ix h w 0) lim dp)



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
