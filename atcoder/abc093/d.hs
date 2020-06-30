{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Bits

main :: IO ()
main = do
    q <- readLn
    replicateM_ q $ do
        [a, b] <- map read.words <$> getLine
        print $ solve a b

solve :: Int -> Int -> Int
solve a b
    | a > b = solve b a
    | a == b = a + b - 2
    | fa 1 >= fb 1 = (a - 1) + fb 1
    | otherwise = (a - 1) + dd + max 0 (fb (dd + 1))
  where
    fa d = a + d
    fb d = b - quot (d * b + fa d) (fa d)
    dd = upperBound 1 b $ \d -> fa d < fb d

-- | assert (p high)
lowerBound :: Int -> Int -> (Int -> Bool) -> Int
lowerBound low high p = go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        mid = low + unsafeShiftR (high - low) 1
{-# INLINE lowerBound #-}

-- | assert (p low)
upperBound :: Int -> Int -> (Int -> Bool) -> Int
upperBound low high p
    | p high = high
    | otherwise = lowerBound low high (not.p) - 1
{-# INLINE upperBound #-}
