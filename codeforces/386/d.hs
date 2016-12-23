main=getLine>>=putStrLn.f.map read.words
f[_,k,a,b]
  | a <= b, div (b + a) (a + 1) > k = "NO"
  | a <= b, (q, r) <- divMod b (a + 1) = merge (g 'B' q r a b) (h 'G' a)
  | b <= a, div (a + b) (b + 1) > k = "NO"
  | b <= a, (q, r) <- divMod a (b + 1) = merge (g 'G' q r b a) (h 'B' b)
g c q 0 low high = replicate (low + 1) (replicate q c)
g c q r low high = replicate r (replicate (q + 1) c) ++ replicate (low + 1 - r) (replicate q c)
h c low = replicate low [c]

merge (xs:xss) (ys:yss) = xs ++ ys ++ merge xss yss
merge [xs] _ = xs
merge _ _ = ""
