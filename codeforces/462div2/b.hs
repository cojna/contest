main=readLn>>=putStrLn.f
f k|k>2*18="-1"
f k = replicate q '8' ++ replicate r '4'
  where
    (q,r) = divMod k 2
