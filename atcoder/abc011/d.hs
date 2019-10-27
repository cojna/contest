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
    [n, d] <- map read.words <$> getLine :: IO [Int]
    [x, y] <- map read.words <$> getLine :: IO [Int]
    print $ solve1000 n d x y

solve1000 :: Int -> Int -> Int -> Int -> Double
solve1000 n d x y
    | rx > 0 || ry > 0 = 0.0
    | otherwise = sum $ map prob [0..n]
  where
    !(qx, rx) = divMod x d
    !(qy, ry) = divMod y d
    prob h
        | all (>= 0) [px, nx, py, ny]
        , even (h + qx)
        , even (n - h + qy)
        = exp $ sum
            [ logComb n h
            , logComb h px
            , logComb (n - h) py
            , - fromIntegral n * log 4
            ]
        | otherwise = 0.0
      where
        px = div (h + qx) 2
        nx = h - px
        py = div (n - h + qy) 2
        ny = (n - h) - py

#define LOG_FACT_CACHE_SIZE 1024

logFactCache :: U.Vector Double
logFactCache = U.scanl' (+) 0.0
    $ U.generate LOG_FACT_CACHE_SIZE (log.fromIntegral.(+1))
{-# NOINLINE logFactCache #-}

logFact :: Int -> Double
logFact = U.unsafeIndex logFactCache
{-# INLINE logFact #-}

logPerm :: Int -> Int -> Double
logPerm n k = logFact n - logFact k
{-# INLINE logPerm #-}

logComb :: Int -> Int -> Double
logComb n k = logFact n - logFact k - logFact (n - k)
{-# INLINE logComb #-}


ix :: Int -> Int -> Int -> Int -> Int
ix x y z w
    = unsafeShiftL x 15
    .|. unsafeShiftL y 10
    .|. unsafeShiftL z 5
    .|. w

solve30 :: Int -> Int -> Int -> Int -> Double
solve30 n d x y
    | mod x d > 0 || mod y d > 0 = 0.0
    | otherwise = res
  where
    !x' = div x d
    !y' = div y d
    res = runST $ do
        dp <- UM.replicate (32 ^ 4) 0.0
        res <- UM.replicate 1 0.0
        UM.write dp (ix 0 0 0 0) 1.0
        rep 31 $ \i -> do
            rep 31 $ \j -> do
                rep 31 $ \k -> do
                    rep 31 $ \l -> do
                        p0 <- UM.unsafeRead dp (ix i j k l)
                        let !p = p0 * 0.25
                        UM.unsafeModify dp (+p) (ix (i+1) j k l)
                        UM.unsafeModify dp (+p) (ix i (j+1) k l)
                        UM.unsafeModify dp (+p) (ix i j (k+1) l)
                        UM.unsafeModify dp (+p) (ix i j k (l+1))
                        when (i - j == x' && k - l == y' && i + j + k + l == n) $ do
                            UM.unsafeModify res (+p0) 0
        UM.read res 0

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
