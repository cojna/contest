main=interact$unlines.foldl(\x(c,d)->map(++[c|d<'v'])x++[c<$x!!0|'>'<d])["a"].zip['b'..]