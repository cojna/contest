{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [n, _] <- map read.words <$> getLine
    coins <- map read.words <$> getLine
    print $ solve n coins

inf :: Int
inf = 0x3f3f3f3f

solve :: Int -> [Int] -> Int
solve n coins = runST $ do
    dp <- UM.replicate (n+1) inf
    UM.write dp 0 0
    forM_ coins $ \coin ->
        for coin (UM.length dp) $ \i -> do
            x <- (+1) <$> UM.unsafeRead dp (i-coin)
            unsafeModify dp i (min x)
    UM.read dp n

-------------------------------------------------------------------------------
rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for !s !t = U.forM_ $ U.generate (t - s) (+s)
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

unsafeModify :: (PrimMonad m, GM.MVector mv a)
             => mv (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeModify v i f = GM.unsafeRead v i >>= GM.unsafeWrite v i . f
{-# INLINE unsafeModify #-}
