main=interact$show.f.map read.words
f(n:x:l)|s<-sum[1|a<-0#l,a<=100*x],s<=n=s|0<1= -1
s#(v:p:l)=s:(s+v*p)#l;s#_=[s]