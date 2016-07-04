{-# LANGUAGE BangPatterns #-}

import          Control.Monad
import           Control.Monad.ST
import           Data.Bits
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
  [n, k] <- map read.words <$> getLine
  ws <- U.fromList.map read.words <$> getContents
  print $ solve n k ws

solve :: Int -> Int -> U.Vector Int -> Int
solve n k ws = runST $ do
    dp <- UM.replicate (shiftL 1 n) 0

    for 1 (shiftL 1 n) $ \s -> do
        forM_ (takeWhile (/= 0) $ iterate (\x -> (x - 1) .&. s) s) $ \t -> do
            dpt <- UM.unsafeRead dp t
            let rest = s `xor` t
            let dps = dpt + k - (U.unsafeIndex table (t .|. rest) - U.unsafeIndex table t - U.unsafeIndex table rest)
            UM.unsafeModify dp (max dps) s
    UM.unsafeRead dp (shiftL 1 n - 1)
  where
    table :: U.Vector Int
    !table = U.create $ do
        tbl <- UM.replicate (shiftL 1 n) 0
        rep (shiftL 1 n) $ \set -> do
            let indices =
                  [ (i, j)
                  | i <- [0..n-2]
                  , j <- [i+1..n-1]
                  , testBit set i
                  , testBit set j
                  ]
            s <- foldM `flip` 0 `flip` indices $ \acc (i, j) -> do
                return $! (+ acc) . U.unsafeIndex ws $ i * n + j
            UM.unsafeWrite tbl set s
        return tbl


rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for !s !t = U.forM_ $ U.generate (t - s) (+s)
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

