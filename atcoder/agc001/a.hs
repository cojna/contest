import           Data.List
main=getContents>>=print.f.map read.words
f(n:xs)=g 0$sort xs
g a (x:y:xs)=g(a+min x y)xs
g a _=a
