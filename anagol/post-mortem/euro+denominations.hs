m@main=readLn>>=putStrLn.init.f>>m
s=1:2:5:map(10*)s
f n=last$"":[shows x"+"++f(n-x)|x<-take 15s,x<=n]