import Data.List
main=interact$f.sort.map read.words
f[x,y]|(q,0)<-divMod(y-x)2,x>=q,y>=3*q,mod(x-q)4<1="Yes"|0<1="No"