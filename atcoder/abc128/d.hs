import Data.List
main=interact$show.f.map read.words
f(n:k:l)=maximum[sum$map(max 0)x++y|i<-[0..k],j<-[0..k-i],i+j<=n,let(x,y)=splitAt(k-i-j).sort$take i l++drop(n-j)l]