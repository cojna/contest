main=interact$f.map read.words
f(_:l)=last$"Yes":["No"|x<-l,2*x>=sum l]