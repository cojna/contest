import Data.List
main=getContents>>=print.f 0.reverse.sort.tail.map read.words
f a(x:y:l)=f(a+x-y)l
f a l=a+sum l