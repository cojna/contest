main=readLn>>=print.f
f n=sum[1|x<-[1..100],rem x n>0]
