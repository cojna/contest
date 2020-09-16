main=getContents>>=print.fst.foldl f(0,e).tail.lines;e='.':e
f(s,x)y=(sum$s:zipWith(#)x y,y)
_#'.'=0
'o'#'o'=0
_#_=1