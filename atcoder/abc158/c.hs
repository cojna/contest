main=interact$show.head.f.map read.words;f l=[x|x<-[1..9^4],[div(y*x)100|y<-[8,10]]==l]++[-1]