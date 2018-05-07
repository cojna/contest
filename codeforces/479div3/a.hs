main=interact$show.solve.map read.words
solve [n, k] = iterate step n !! k
  where
    step n
      | mod n 10 == 0 = div n 10
      | otherwise = n - 1
