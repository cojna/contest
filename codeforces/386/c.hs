import           Control.Applicative

main = do
    [s,x1,x2] <- map read.words <$> getLine
    [t1,t2] <- map read.words <$> getLine
    [p,d] <- map read.words <$> getLine
    print $ solve s x1 x2 t1 t2 p d

solve s x1 x2 t1 t2 p d
  | t1 >= t2 = t2 * abs (x2 - x1)
  | x1 == x2 = 0
  | x1 == p, x1 < x2 = f $ if d == 1 then x2 - x1 else p + x2
  | x1 == p, x2 < x1 = f $ if d == 1 then (s - p) + (s - x2) else x1 - x2
  | x1 <= x2, x2 <= p = f $ if d == 1 then (s - p) + s + x2 else p + x2
  | x1 <= p, p <= x2 = f $ if d == 1 then (s - p) + s + x2 else p + x2
  | p <= x1, x1 <= x2 = f $ if d == 1 then x2 - p else p + x2
  | x2 <= x1, x1 <= p = f $ if d == 1 then (s - p) + (s - x2) else p - x2
  | x2 <= p, p <= x1 = f $ if d == 1 then (s - p) + (s - x2) else p + s + (s - x2)
  | p <= x2, x2 <= x1 = f $ if d == 1 then (s - p) + (s - x2) else p + s + (s - x2)
 where
  f x = min (t2 * abs (x2 - x1)) (t1 * x)
