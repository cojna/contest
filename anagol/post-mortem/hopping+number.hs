l=[1..8]
main=mapM putStrLn[[a,a+d..a+d*n]>>=show|n<-l,a<-l,d<-l,a+d*n<=9,d<8]