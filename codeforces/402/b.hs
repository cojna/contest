import Control.Applicative

main = do
  [cs, k] <- words <$> getLine
  print $ solve cs $ read k

solve :: String -> Int -> Int
solve cs k = solve0 cs `min` solve1 cs
  where
    solve0 cs = length cs - 1
    solve1 cs = go 0 k $ reverse cs
      where
        go acc 0 _ = acc
        go acc k ('0':cs) = go acc (k-1) cs
        go acc k (_:cs) = go (acc+1) k cs
        go _ _ _ = maxBound
