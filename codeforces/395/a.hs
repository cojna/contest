main=interact$show.f.map read.words
f[n,m,z]=sum[1|c<-[n,n+n..z],rem c m<1]
