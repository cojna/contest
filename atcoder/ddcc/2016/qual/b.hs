main=getLine>>=print.f.map read.words
f[r,n,m] = sum[g i `max` g (i-m)|i<-[1..n+m-1]]
  where
    g i
      | 0 < i, i < n = 2 * sqrt (fromIntegral(r*r)-(fromIntegral (2*r*i)/fromIntegral n-fromIntegral r)^2)
      | otherwise = 0.0
