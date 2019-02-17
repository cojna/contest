main=interact$show.f.map(tail.words).tail.lines
f l=sum[1|i<-[1..30],all(elem$show i)l]