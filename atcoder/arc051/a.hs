main=interact$f.map read.words
f[x1, y1, r, x2, y2, x3, y3]=unlines[red,blue]
  where
    red | and [x2<=x1-r,x1+r<=x3,y2<=y1-r,y1+r<=y3] = "NO"
        | otherwise = "YES"
    blue | and [(x - x1) ^ 2 + (y - y1) ^ 2 <= r * r|x<-[x2, x3], y<-[y2, y3]] = "NO"
         | otherwise = "YES"
