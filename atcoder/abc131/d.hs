{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

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
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    n <- readLn
    xys <- U.unfoldrN n parseInt2 <$> C.getContents
    putStrLn.bool"No""Yes"$solve n xys

solve :: Int -> U.Vector (Int, Int) -> Bool
solve n (radixSortNonNegative.U.map swap->xys)
    = U.and
    . U.zipWith (<=) (U.postscanl' (+) 0 $ U.map snd xys)
    $ U.map fst xys

-------------------------------------------------------------------------------
class Word64Encode a where
    encode64 :: a -> Word64
    decode64 :: Word64 -> a
    encodeNonNegative64 :: a -> Word64
    encodeNonNegative64 = encode64
    decodeNonNegative64 :: Word64 -> a
    decodeNonNegative64 = decode64

instance Word64Encode Int where
    encode64 x = unsafeCoerce $ x + 0x3fffffffffffffff
    decode64 x = unsafeCoerce x - 0x3fffffffffffffff
    encodeNonNegative64 = unsafeCoerce
    decodeNonNegative64 = unsafeCoerce

instance Word64Encode (Int, Int) where
    encode64 (x, y) = unsafeCoerce
        $ unsafeShiftL (x + 0x3fffffff) 31 .|. (y + 0x3fffffff)
    decode64 xy = unsafeCoerce (x, y)
      where
        !x = unsafeShiftR xy 31 - 0x3fffffff
        !y = (xy .&. 0x7fffffff) - 0x3fffffff
    encodeNonNegative64 (x, y) = unsafeCoerce $ unsafeShiftL x 31 .|. y
    decodeNonNegative64 xy = unsafeCoerce (x, y)
      where
        !x = unsafeShiftR xy 31
        !y = xy .&. 0x7fffffff

radixSort64 :: U.Vector Word64 -> U.Vector Word64
radixSort64 v = F.foldl' step v [0, 16, 32, 48]
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step v k = U.create $ do
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

radixSort
    :: (U.Unbox a, Word64Encode a)
    => U.Vector a -> U.Vector a
radixSort = U.map decode64 . radixSort64 . U.map encode64
{-# INLINE radixSort #-}

radixSortNonNegative
    :: (U.Unbox a, Word64Encode a)
    => U.Vector a -> U.Vector a
radixSortNonNegative
    = U.map decodeNonNegative64
    . radixSort64
    . U.map encodeNonNegative64
{-# INLINE radixSortNonNegative #-}

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
