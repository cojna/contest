main=interact$show.f.map(map read.words).lines
f(h:b:l)=sum[1|a<-l,sum(zipWith(*)a b)+h!!2>0]