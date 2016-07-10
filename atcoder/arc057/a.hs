main=getLine>>=print.f.map read.words
f[a,0]=n-a
f[a,k]=fst.head.filter((>=n).snd)$iterate(\(i,x)->(i+1,x+1+k*x))(0,a)
n=2*10^12
