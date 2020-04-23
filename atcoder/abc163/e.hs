{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances, KindSignatures, LambdaCase, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections      #-}
{-# LANGUAGE TypeApplications, TypeFamilies, ViewPatterns             #-}

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

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- U.unfoldrN n (runParser int) <$> C.getLine
    print $ solve n xs

encode :: Int -> Int -> Word64
encode i x = unsafeCoerce $ unsafeShiftL (0x7fffffff - x) 32 .|. i

decode :: Word64 -> (Int, Int)
decode xi =
    let !x = 0x7fffffff - unsafeShiftR xi 32
        !i = xi .&. 0xffffffff
    in unsafeCoerce (i, x)

pre :: U.Vector Int -> U.Vector Word64
pre = radixSort64.U.imap encode

ix :: Int -> Int -> Int
ix i j = unsafeShiftL i 11 .|. j
{-# INLINE ix #-}

solve :: Int -> U.Vector Int -> Int
solve n (pre -> xos) = U.maximum $ U.ifoldl' step (U.singleton 0) xos
  where
    step :: U.Vector Int -> Int -> Word64 -> U.Vector Int
    step prev i xo = U.create $ do
        dp <- UM.replicate (i + 2) 0
        flip U.imapM_ prev $ \l dpl -> do
            let !newl = dpl + score xo l
            UM.unsafeModify dp (max newl) (l + 1)
            let !r = i - l
            let !newr = dpl + score xo (n-1-r)
            UM.unsafeModify dp (max newr) l
        return dp
    score xo pos = case decode xo of
        (o, x) -> x * abs (pos - o)


radixSort64 :: U.Vector Word64 -> U.Vector Word64
radixSort64 v = step 48 $! step 32 v
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step k v = U.create $ do
        pref <- U.unsafeThaw
            . U.prescanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate 0x10000 0)
            $ U.map (flip (,) 1 . mask k) v
        res <- UM.unsafeNew $ U.length v
        U.forM_ v $ \x -> do
            let !masked = mask k x
            i <- UM.unsafeRead pref masked
            UM.unsafeWrite pref masked $ i + 1
            UM.unsafeWrite res i x
        return res
{-# INLINE radixSort64 #-}

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
