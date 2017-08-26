main = getContents >>= print.f.map read.words
f[q,h,s,d,n]
    | even n = div n 2 * y
    | otherwise = min (n * x) $ x + div n 2 * y
  where
    x = minimum xs
    y = minimum ys
    xs = [4*q, 2*h, s]
    ys = [8*q, 4*h, 2*s, d]
