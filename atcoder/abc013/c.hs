main=interact$f.map read.words;f[n,h,a,b,c,d,e]=shows(minimum[a*x+c*y|x<-[0..n],let y=max 0$div(e*(n-x)-b*x-h+d+e)(d+e),x+y<=n])"\n"