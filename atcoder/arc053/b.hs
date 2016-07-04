{-# LANGUAGE BangPatterns #-}

import Data.List

main=getLine>>=print.f

f cs
  | null os || length os == 1 = length cs
  | otherwise = 2 * upperBound 0 100000 isValid - 1
  where
    (es, os) = partition even.map length.group $ sort cs
    isValid x = sum [2*x-1-o|o<-os] <= s
    !s = sum es

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        mid = (low + high) `quot` 2
{-# INLINE lowerBound #-}

upperBound :: (Integral i) => i -> i -> (i -> Bool) -> i
upperBound low high p
    | p high = high
    | otherwise = lowerBound low high (not.p) - 1
{-# INLINE upperBound #-}