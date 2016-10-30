main=interact$maybe"No\n"(unlines.("Yes":).map show).f.map read.words
f[n,x]
  | x == 1 || x == 2*n-1 = Nothing
  | otherwise = let (xs, ys) = splitAt (n-1) $ [1..x-1] ++ [x+1..2*n-1]
                in Just $ reverse xs ++ x:reverse ys