main=readLn>>=print.(l!!)
l=2:1:zipWith(+)l(tail l)
