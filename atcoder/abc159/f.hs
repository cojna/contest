{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances, KindSignatures, LambdaCase, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms, RecordWildCards, ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections, TypeFamilies, ViewPatterns                #-}

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
import           Data.Primitive
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

pattern MOD# :: Int#
pattern MOD# = 998244353#
pattern MOD :: Int
pattern MOD = I# MOD#

main :: IO ()
main = do
    [n, s] <- map read.words <$> getLine :: IO [Int]
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    print $ solve n s xs

solve :: Int -> Int -> U.Vector Int -> Int
solve n s xs
    = fst
    $ U.ifoldl' step (0, U.replicate (s + 1) 0) xs
  where
    step :: (Int, U.Vector Int) -> Int -> Int -> (Int, U.Vector Int)
    step (!acc, !dp) i x
        | x > s = (acc, dp)
        | s == x = (acc +% (i + 1) *% (n - i), dp)
        | otherwise = (,) (acc +% (n - i) *% (dp U.! (s - x)))
            . U.zipWith (+%) dp
            $ U.replicate x 0 U.++ U.cons (i + 1) (U.tail dp)

infixl 7 *%
infixl 6 +%, -%

(+%) :: Int -> Int -> Int
(I# x#) +% (I# y#) = case x# +# y# of
    r# -> I# (r# -# ((r# >=# MOD#) *# MOD#))
{-# INLINE (+%) #-}

(-%) :: Int -> Int -> Int
(I# x#) -% (I# y#) = case x# -# y# of
    r# -> I# (r# +# ((r# <# 0#) *# MOD#))
{-# INLINE (-%) #-}

(*%) :: Int -> Int -> Int
(I# x#) *% (I# y#) = I# ((x# *# y#) `remInt#` MOD#)
{-# INLINE (*%) #-}

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
