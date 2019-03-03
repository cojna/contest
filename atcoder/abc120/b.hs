main=interact$show.f.map read.words
f[a,b,k]|g<-gcd a b=[x|x<-g:[g,g-1..],rem a x+rem b x<1]!!k