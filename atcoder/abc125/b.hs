main=interact$show.f.map(map read.words).lines;f[_,x,y]=sum[d|d<-zipWith(-)x y,d>0]