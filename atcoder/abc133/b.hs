main=interact$show.f.map(map read.words).tail.lines;f l=sum[1|x<-l,y<-l,x<y,i<-[1..999],sum[(a-b)^2|(a,b)<-zip x y]==i*i]