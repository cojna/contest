import           Data.List
main=getContents>>=print.solve.tail.map read.words
solve xs
  | mod s 10 > 0 = s
  | (r:_) <- sort rs = s - r
  | otherwise = 0
  where
    s = sum xs
    rs = [ x |x<-xs, mod x 10 > 0]
