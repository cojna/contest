main :: IO ()
main=getLine>>=print.solve.map read.words

solve :: [Integer] -> Integer
solve [n,x]
  | 2 * x < n = go (n+2*x) x (n - 2 * x)
  | 2 * x == n = 3 * x
  | otherwise = go n x (n-x)
  where
    go acc x y
      | x > y = go acc y x
      | x == y = x
      | otherwise = case divMod y x of
          (q, 0) -> acc + 2 * q * x - x
          (q, r) -> go (acc + 2 * q * x) x r
