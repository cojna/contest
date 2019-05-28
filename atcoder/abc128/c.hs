main=interact$show.f.map(map read.words).lines
f([n,m]:z)=sum[1|s<-mapM(:[0])[1..n],and$zipWith(\(k:l)p->even$p+sum[1|x<-l,elem x s])z$z!!m]