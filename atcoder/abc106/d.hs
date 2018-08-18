
{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
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
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, m, q] <- map read.words <$> getLine :: IO [Int]
    (lrs, qs) <- U.splitAt m.U.unfoldrN (m + q) parseInt2 <$> B.getContents
    putStr.unlines.map show $ solve n lrs qs

width :: Int
width = 512

solve :: Int -> U.Vector (Int, Int) -> U.Vector (Int, Int) -> [Int]
solve n lrs qs = U.toList $ U.map (U.unsafeIndex table . uncurry ix1) qs
  where
    ix1 x y = ix (x - 1) (y - 1)
    ix x y = x * width + y
    !table = U.create $ do
        t <- UM.replicate (width * (n + 1)) 0
        U.forM_ lrs $ \(l, r) -> do
            UM.unsafeModify t (+1) (ix 0 (r - 1))
            UM.unsafeModify t (subtract 1) (ix l (r - 1))

        rep (n + 1) $ \i ->
            for 1 (n + 1) $ \j -> do
                acc <- UM.unsafeRead t (ix i (j - 1))
                UM.unsafeModify t (+acc) (ix i j)
        for 1 (n+1) $ \i ->
            rep (n + 1) $ \j -> do
                acc <- UM.unsafeRead t (ix (i - 1) j)
                UM.unsafeModify t (+acc) (ix i j)

        return t


-------------------------------------------------------------------------------
type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)

rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for !s !t = U.forM_ $ U.generate (t - s) (+s)
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}
