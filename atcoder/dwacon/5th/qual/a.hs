main=interact$show.f.map read.words
f(n:l)=snd$minimum[(abs$n*x-sum l,i)|(i,x)<-zip[0..]l]
