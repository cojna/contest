main=interact$show.f.map read.tail.words;f(x:y:z:l)=sum[x^0|min x z<y,y<max x z]+f(y:z:l);f _=0