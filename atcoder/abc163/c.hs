import Data.List;main=interact$f.map read.words;f(n:l)=do _:g<-group.sort$[1..n]++l;shows(length g)"\n"