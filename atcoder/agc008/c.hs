main=interact$show.f.map read.words
f[i,o,t,j,l,s,z] = o + g [i, j, l]
g[i,j,l] = case map (`mod`2) [i, j, l] of
    [0, 0, 0] -> s
    [1, 0, 0] -> s - 1
    [0, 1, 0] -> s - 1
    [0, 0, 1] -> s - 1
    [1, 1, 0] -> s - 2 + sum[1|l>0]
    [1, 0, 1] -> s - 2 + sum[1|j>0]
    [0, 1, 1] -> s - 2 + sum[1|i>0]
    [1, 1, 1] -> s
  where
    s = i + j + l