main=interact$unlines.map(unwords.map show).f.map read.words
f(_:k:s)=[[sum$zipWith(-)(k#s)s],k#s]
k#(x:y:s)|x+y<k=x:k#((k-x):s)|0<1=x:k#(y:s)
k#s=s
