import GHC.Integer.GMP.Internals
main=interact$show.f.map read.words
m=1000000007
f[n,a,b]=mod(powModInteger 2 n m-1-n%a-n%b)m
n%r=foldl(\x y->rem(x*y)m)1[(n+1-i)*recipModInteger i m|i<-[1..r]]