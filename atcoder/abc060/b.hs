main=interact$f.map read.words;f[a,b,c]=last$"NO":["YES"|x<-[0..b],mod(a*x)b==c]