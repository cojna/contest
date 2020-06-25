{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
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
    s <- U.unfoldr B.uncons <$> B.getLine
    t <- U.unfoldr B.uncons <$> B.getLine
    print $ solve s t

nothing :: Int
nothing = -1

solve :: U.Vector Word8 -> U.Vector Word8 -> Int
solve s t
    | validate freqS freqT = succ . fst . U.foldl' step (pos0, U.head t) $ U.tail t
    | otherwise = -1
 where
    freqS = U.accumulate (+) (U.replicate 256 0)
        $ U.map (\w -> (fromIntegral w, 1)) s
    freqT = U.accumulate (+) (U.replicate 256 0)
        $ U.map (\w -> (fromIntegral w, 1)) t
    !next = buildNext s
    !lenS = U.length s
    pos0 = U.length $ U.takeWhile (/= U.head t) s
    step (!pos, !prev) w
        | prev /= w = (pos + diffPos, w)
        | otherwise = (pos + 1 + diffPos, w)
      where
        diffPos = mod (next V.! fromIntegral w U.! modPos - modPos) lenS
        !modPos
            | prev == w = rem (pos + 1) lenS
            | otherwise = rem pos lenS

buildNext :: U.Vector Word8 -> V.Vector (U.Vector Int)
buildNext xs = V.generate 256 (row.fromIntegral)
  where
    !xsxs = xs U.++ xs
    !n = U.length xs
    row x = U.take n.U.scanr' f nothing $ U.indexed xsxs
      where
        f (i, y) next
            | y == x = rem i n
            | otherwise = next


validate :: U.Vector Int -> U.Vector Int -> Bool
validate freqS freqT = U.and $ U.zipWith f freqS freqT
  where
    f s t = t == 0 || s > 0

-------------------------------------------------------------------------------
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