main=interact$f.last.lines
f('o':'g':'o':cs)="***"++g cs
f (c:cs)=c:f cs
f ""=""
g('g':'o':cs)=g cs
g cs=f cs
