main=interact$show.f.map read.words;f[a,b,c,s]=sum[1|[x,y,z]<-mapM(\x->[0..x])[a,b,c],50*(10*x+2*y+z)==s]
