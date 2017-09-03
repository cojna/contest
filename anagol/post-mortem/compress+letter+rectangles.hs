main=interact$f.lines
f[s]=s
f((c:x):l)|t<-tail<$>l,x/=t!!0=c:'>':f(x:t)|0<1=c:'v':f l