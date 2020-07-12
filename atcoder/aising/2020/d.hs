import Foreign
f 0=0
f x=1+f(rem x.toInteger$popCount x)
g[n,s]=unlines[show.f$foldl(\x c->2*x+read[c])0s`xor`bit(-i)|i<-[1-read n..0]]
main=interact$g.lines