main=interact$f.map(map read.words).lines;f(h:l)|(x,y)<-splitAt(h!!0)l=do[a,b]<-x;shows(snd$minimum[(abs(c-a)+abs(d-b),i)|(i,[c,d])<-zip[1..]y])"\n"