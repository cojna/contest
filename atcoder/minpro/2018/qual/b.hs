main=getLine>>=print.f.map read.words
f :: [Int] -> Int
f[x,0]          = x + 1
f[1000000000,9] = 2000000000
f[_,9]          = 1000000000
f[x,k]          = [y|y<-map(*(10^k))[1..],x<y]!!0
