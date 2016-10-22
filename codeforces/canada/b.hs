main=getLine>>=print.f.span(\c->'0'<=c&&c<='9')
f(x,[y])
  | r == 0 = block * q + c
  | r == 1 = block * q + 6 + 1 + c
  | r == 2 = block * q + c
  | otherwise = block * q + 6 + 1 + c
  where
    block = 16
    n = read x - 1:: Integer
    c = [i|(i,z)<-zip[1..]"fedabc",z==y]!!0
    (q, r) = divMod n 4
