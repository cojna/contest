{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [n, w] <- map read.words <$> getLine :: IO [Int]
    vws <- U.unfoldrN n parseInt2 <$> B.getContents
    print $ solve w vws

type Value = Int
type Weight = Int

solve :: Int -> U.Vector (Value, Weight) -> Value
solve w vws = runST $ do
    dp <- UM.replicate (w + 1) 0

    U.forM_ vws $ \(vi, wi) ->
        rev (UM.length dp) $ \j ->
            when (wi <= j) $ do
                x <- (+vi) <$> UM.unsafeRead dp (j - wi)
                unsafeModify dp j (max x)

    UM.unsafeRead dp w

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

type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)