main=do l<-map read.words<$>getLine;k<-readLn;print$sum l+maximum l*(2^k-1)
