import Foreign
main=interact$show.f.map read.words
f[a,b]=g b`xor`g(a-1)
g x=[x,1,x+1,0]!!mod x 4