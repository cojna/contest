import Data.Char
main=interact$f.lines;f[x,y]|x==y="same"|map toLower x==map toLower y="case-insensitive"|0<1="different"