import Data.Graph;main=interact$show.f.map read.words;f(n:_:l)=length.dff.buildG(1,n)$zip(p l)l;p(x:y:z:l)=y:x:z:p l;p l=l