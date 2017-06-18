main=getLine>>=print.f
f[h1,h2,_,m1,m2] = length.takeWhile (not.isPalindrome.show) $ iterate suc (T h m)
  where
    h, m :: Int
    h = read[h1, h2]
    m = read[m1, m2]

isPalindrome xs = xs == reverse xs

data T = T Int Int

instance Show T where
    show (T h m) = hh ++ mm
      where
        hh | h <= 9 = "0" ++ show h
           | otherwise = show h
        mm | m <= 9 = "0" ++ show m
           | otherwise = show m

suc (T 23 59) = T 0 0
suc (T h 59) = T (h + 1) 0
suc (T h m) = T h (m + 1)
