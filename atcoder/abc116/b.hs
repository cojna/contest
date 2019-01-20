main=readLn>>=print.f[0]
f s x|elem x s=length s|0<1=f(x:s)$cycle[div x 2,3*x+1]!!x