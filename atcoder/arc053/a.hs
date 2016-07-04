main=getLine>>=print.f.map read.words
f[1,1]=0
f[1,w]=w-1
f[h,1]=h-1
f[h,w]=2*(h-1)*(w-1)+(h-1)+(w-1)