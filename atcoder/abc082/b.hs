import Data.List;main=interact$f.lines;f[s,t]|sort s<reverse(sort t)="Yes"|0<1="No"