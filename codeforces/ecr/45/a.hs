main=interact$show.f.map read.words
f :: [Integer] -> Integer
f[n,m,a,b]
   | mod n m == 0 = 0
   | otherwise = minimum [a * (m - mod n m), b * mod n m]
