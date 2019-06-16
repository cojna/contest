{-# LANGUAGE BangPatterns #-}

import           Data.Bits
import qualified Data.IntMap.Strict as IM
import qualified Data.List          as L

main :: IO ()
main = do
    [x, p, a, b] <- map read.words <$> getLine
    print $ solve x p a b

solve :: Int -> Int -> Int -> Int -> Int
solve x p a b
    | b - a > p - 1 = 1
    | n + 1 < 10 ^ 8 = ub
    | otherwise = minimum [y | y<-[1..ub], Just z<- [logMod x y p], a <= z, z <= b]
  where
    xa = powMod x a p
    n = b - a
    !ub = minimum . take (min (10^8) $ n + 1) $ iterate (\acc -> acc * x `rem` p) xa

powMod :: (Integral a, Integral b, Bits b) => a -> b -> a -> a
powMod x n m
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (recipMod x m) (-n)
  where
    go !acc !y !i
        | i .&. 1 == 0 = go acc (y * y `rem` m) (unsafeShiftR i 1)
        | i == 1 = acc * y `rem` m
        | otherwise = go (acc * y `rem` m) (y * y `rem` m) (unsafeShiftR (i - 1) 1)
{-# INLINE powMod #-}

recipMod :: (Integral a) => a -> a -> a
recipMod x m = go x m 1 0
  where
    go !a !b !u !v
        | b > 0 = case a `quot` b of
            q -> go b (a - (q * b)) v (u - (q * v))
        | otherwise = u `mod` m
{-# INLINE recipMod #-}

-- |
-- Baby-step Giant-step
-- @a^x = b (mod p)@　p is prime
-- @O(sqrt P * log P)@
logMod :: Int -> Int -> Int -> Maybe Int
logMod a b p = go 0 b
  where
    !sqrtP = ceiling . sqrt $ fromIntegral p
    !g = powMod a (-sqrtP) p
    babyStep x = a * x `rem` p
    giantStep x = g * x `rem` p

    table :: IM.IntMap Int
    !table = IM.fromList $ zip (iterate babyStep 1) [0..sqrtP]

    go !i !x
        | i < sqrtP = case IM.lookup x table of
            Just j -> Just $! i * sqrtP + j
            Nothing -> go (i + 1) $ giantStep x
        | otherwise = Nothing

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        h = toInteger high
        l = toInteger low
        mid = fromIntegral $ l + div (h - l) 2
{-# INLINE lowerBound #-}
