import Data.List
main=interact$f.map read.words
f(n:a)|elem n$iterate(nub.(>>= \x->map(x+)[0..3]\\a))[0]!!100="YES\n"|0<1="NO\n"