main=interact$show.sum.map(f.read).tail.words
f x|(q,0)<-divMod x 2=1+f q|0<1=0
