main=getLine>>=print.f.map read.words
f[l,x,y,s,d]
    | s == d = 0.0
    | s < d =
        if x < y
        then min ((d - s) / (x + y)) ((l - (d - s)) / (y - x))
        else (d - s) / (x + y)
    | otherwise =
        if x < y
        then min ((l - (s - d)) / (x + y)) ((s - d) / (y - x))
        else (l - (s - d)) / (x + y)
