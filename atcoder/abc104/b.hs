main=getLine>>=putStr.f
f('A':s)|[x|x<-s,x<'a',s>"[",last s>'Z']=="C"="AC"
f _="WA"

