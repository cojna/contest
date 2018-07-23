import           Data.List
main=interact$show.f.map read.words
f l=minimum[abs(y-x)+abs(z-y)|[x,y,z]<-permutations l]
