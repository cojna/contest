main=interact$show.f.map read.words;f(n:l)=maximum[abs$l!!i-l!!j|i<-[0..n],j<-[i+1..n-1]]