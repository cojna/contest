main=getLine>>=print.f.('a':)
f(c:d:cs)=g c d+f(d:cs)
f[_]=0
g x y=min(length $ takeWhile(/=y) a) $length $ takeWhile(/=y) b
  where
    a = [x..'z'] ++ ['a'..]
    b = [x, pred x..'a'] ++ ['z','y'..]
