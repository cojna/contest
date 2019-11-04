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
    n <- readLn :: IO Int
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    print $ solve n xs

solve :: Int -> U.Vector Int -> Int
solve n xs = go (score 1 2 3) 1 [2..n-2] 3
  where
    go !acc q (r:rs) s = go (min acc (score q' r s')) q' rs s'
      where
        q' = moveQ q r
        s' = moveS s r
    go acc _ [] _ = acc

    moveQ q r
        | q + 1 == r = q
        | f q > f (q + 1) = moveQ (q + 1) r
        | otherwise = q
      where
        f i = abs $ query 0 i - query i r

    moveS s r
        | s + 1 == r = s
        | f s > f (s + 1) = moveS (s + 1) r
        | otherwise = s
      where
        f i = abs $ query r i - query i n

    !ps = U.scanl' (+) 0 xs
    -- sum[l..r)
    query :: Int -> Int -> Int
    query l r = U.unsafeIndex ps r - U.unsafeIndex ps l

    score q r s = maximum xs - minimum xs
      where
        !sumP = query 0 q
        !sumQ = query q r
        !sumR = query r s
        !sumS = query s n
        !xs = [sumP, sumQ, sumR, sumS]

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
-- Algorithm.BinarySearch.Int
-------------------------------------------------------------------------------
lowerBoundInt :: Int -> Int -> (Int -> Bool) -> Int
lowerBoundInt low high p = assert (p high) $ go low high where { go !low !high | high <= low = high | p mid = go low mid | otherwise = go (mid + 1) high where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE lowerBoundInt #-}
upperBoundInt :: Int -> Int -> (Int -> Bool) -> Int
upperBoundInt low high p | p high = high | otherwise = lowerBoundInt low high (not . p) - 1
{-# INLINE upperBoundInt #-}
