import Data.List
main=interact$show.f.map read.words
f(n:l)=sum$zipWith(*)[1-n,3-n..]$sort l