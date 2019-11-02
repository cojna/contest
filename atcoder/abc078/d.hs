{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances         #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses   #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeFamilies, ViewPatterns #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Reader
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
    [n, z, w] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    print $ solve n z w xs

solve :: Int -> Int -> Int -> U.Vector Int -> Int
solve n z w xs = runST $ do
    dp <- UM.replicate ((n + 1) * (n + 1)) (-1)
    rep (n + 1) $ \i -> do
        UM.unsafeWrite dp (ix i n) $ score i n
    rep (n + 1) $ \j -> do
        UM.unsafeWrite dp (ix n j) $ score n j
    rev n $ \i -> do
        rev n $ \j -> do
            if i <= j
            then do
                if j - 1 <= i
                then do
                    flip fix (j + 1, minBound) $ \loop (!k, !acc) -> do
                        if k <= n
                        then do
                            !fkj <- UM.unsafeRead dp (ix k j)
                            loop (k + 1, max acc fkj)
                        else UM.unsafeWrite dp (ix i j) acc
                else do
                    UM.unsafeRead dp (ix (i + 1) j)
                        >>= UM.unsafeWrite dp (ix i j)
            else do
                if j == i - 1
                then do
                    flip fix (i + 1, maxBound) $ \loop (!k, !acc) -> do
                        if k <= n
                        then do
                            !fik <- UM.unsafeRead dp (ix i k)
                            loop (k + 1, min acc fik)
                        else UM.unsafeWrite dp (ix i j) acc
                else do
                    UM.unsafeRead dp (ix i (j + 1))
                        >>= UM.unsafeWrite dp (ix i j)
    UM.unsafeRead dp (ix 0 0)
  where
    ix i j = i * (n + 1) + j
    unIx ij = quotRem ij (n + 1)

    score 0 0 = abs (z - w)
    score 0 j = abs (z - xs U.! (j - 1))
    score i 0 = abs (w - xs U.! (i - 1))
    score i j = abs (xs U.! (i - 1) - xs U.! (j - 1))

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
