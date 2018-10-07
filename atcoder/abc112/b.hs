main=interact$f.map read.words
f(_:t:l)|t#l>[0]=show.minimum$t#l|0<1="TLE"
t#(x:y:l)=[x|y<=t]++t#l
t#l=l