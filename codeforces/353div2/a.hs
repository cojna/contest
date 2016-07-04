main=interact$f.map read.words
f[a,b,0]
  |a==b="YES"
  |0<1="NO"
f[a,b,c]
  |r==0, q>=0="YES"
  |0<1="NO"
  where
    (q,r) = divMod (b-a) c