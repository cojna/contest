main=interact$show.f.map read.words;f(n:l)=sum[1|(i,x)<-zip[1..]l,odd i,odd x]