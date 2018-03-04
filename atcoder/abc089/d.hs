{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
    [h,w,d] <- map read.words <$> getLine :: IO [Int]
    us <- replicateM h $ do
        U.unfoldrN w (B.readInt.B.dropWhile isSpace) <$> B.getLine
    !q <- readLn :: IO Int
    lrs <- U.unfoldrN q parseInt2 <$> B.getContents
    putStr.unlines.map show.U.toList$ solve h w d us lrs

solve :: Int -> Int -> Int -> [U.Vector Int] -> U.Vector (Int, Int) -> U.Vector Int
solve h w d us lrs = U.map(\(l,r) -> U.unsafeIndex dist (r-1) - U.unsafeIndex dist (l-1)) lrs
  where
    ps :: U.Vector (Int, Int)
    !ps = U.unsafeUpdate_
        (U.replicate (h*w) (0, 0))
        (U.map pred $ U.concat us)
        (U.generate (h*w) (`quotRem`w))
    dist :: U.Vector Int
    !dist = U.create $ do
        dis <- UM.replicate (h*w) 0
        for d (h*w) $ \i -> do
            pd <- UM.unsafeRead dis (i-d)
            let (px, py) = U.unsafeIndex ps (i-d)
            let (x, y) = U.unsafeIndex ps i
            UM.unsafeWrite dis i $ pd + abs(x-px) + abs(y-py)
        return dis
-------------------------------------------------------------------------------
rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for !s !t = U.forM_ $ U.generate (t - s) (+s)
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
