import Data.List
main=interact$show.minimum.f.map read.words
f(n:k:l)=zipWith(-).drop(k-1)<*>id$sort l