main = interact $ show.solve.map read.words
solve[m,b]=maximum[(x+1)*(y+1)*(x+y)`quot`2|x<-[0,m.. m * b],let y = negate x `quot` m + b]
