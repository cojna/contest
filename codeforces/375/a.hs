main=interact$show.f.map read.words
f[a,b,c]=minimum[abs(x-a)+abs(x-b)+abs(x-c)|x<-[-1000..1000]]
