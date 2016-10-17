main=interact$show.f.map read.words
f[b,d,s]=minimum[sum$zipWith(-)[m+x,m+y,m+z][b,d,s]|x<-[0,1],y<-[0,1],z<-[0,1],let m=maximum[b-x,d-y,s-z]]