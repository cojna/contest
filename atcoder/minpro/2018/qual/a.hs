main=getLine>>=putStrLn.f
f('y':'a':'h':x:y:_)|x == y = "YES"
f _                  = "NO"
