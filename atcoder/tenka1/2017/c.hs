main=readLn>>=putStrLn.unwords.map show.head.f
f n=[[x, y, div p q]|x<-[1..3500], y<-[x..3500],let p=n*x*y, let q=4*x*y-n*(x+y),p>0,q>0,rem p q==0,p >= q]
