main=getLine>>=print.sum.f
f(c:s)=1:f(snd$span(>c)s)
f s=[]