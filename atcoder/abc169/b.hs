main=interact$show.go 1.map read.tail.words
go :: Integer -> [Integer] -> Integer
go acc (x:xs)
    | acc * x > lim = if elem 0 xs then 0 else (-1)
    | otherwise = go (acc * x) xs
go acc [] = acc
lim = 10^18