main=interact$f.map read.words
f[s,v1,v2,t1,t2]
    | x < y = "First"
    | x == y = "Friendship"
    | otherwise = "Second"
  where
    x = s * v1 + 2 * t1
    y = s * v2 + 2 * t2
