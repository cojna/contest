main=interact$show.f.map read.words
f[n,x,y,z,w]=n*(max 0 $ minimum[n-y+z,n-x-y+z+w,n-x+w,n]-maximum[1,1-y+z,1-x-y+z+w,1-x+w]+1)