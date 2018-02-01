main=getContents>>=print.minimum.f.map read.words
f(_:m:l)=g l
  where
    g(x:y:z)=(x/y*m):g z
    g _ = []
