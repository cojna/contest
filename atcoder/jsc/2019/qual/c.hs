main = interact $ show . f . lines
f[n, s] = go (foldl (*%) 1 [1..read n]) 0 s
go res open _ | res == 0 || open < 0 = 0
go res open ('W':cs)
  | even open = go (res *% open) (open - 1) cs
  | otherwise = go res (open + 1) cs
go res open ('B':cs)
  | even open = go res (open + 1) cs
  | otherwise = go (res *% open) (open - 1) cs
go res 0 [] = res
go _ _ [] = 0
x *% y = x * y `rem` 1000000007