main=interact$f.map read.words;f[x,y]|[]<[i|i<-[0..x],2*i+4*(x-i)==y]="Yes"|0<1="No"