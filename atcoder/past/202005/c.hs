main=interact$f.map read.words
f[a,1,n]=show a
f[a,r,n]|n>64||a*r^pred n>10^9="large"|0<1=show$a*r^pred n