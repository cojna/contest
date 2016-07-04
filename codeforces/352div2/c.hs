{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Int

main = do
  [ax,ay,bx,by,tx,ty] <- map read.words <$> getLine :: IO [Integer]
  n <- readLn
  xys <- parse.map readInt.B.words <$> B.getContents
  print $ solve n (ax,ay) (bx,by) (tx,ty) xys

solve 1 axy bxy txy [xy] = minimum [dist axy xy+dist txy xy, dist bxy xy+dist txy xy]
solve n axy bxy txy xys = minimum $ [calc1 axy, calc1 bxy] ++ [calc2 a b|(a,b)<-[(a1,b1),(a2,b1),(a1,b2)], a /= b]
  where
    (a1:a2:_) = map snd $ sort[(dist axy xy - dist txy xy, xy)|xy<-xys]
    (b1:b2:_) = map snd $ sort[(dist bxy xy - dist txy xy, xy)|xy<-xys]

    !s = 2*sum[dist txy xy |xy<-xys]
    calc1 a0 = s + minimum[dist a0 xy - dist txy xy|xy<-xys]
    calc2 a1 b1 = s + (dist axy a1 - dist txy a1) + (dist bxy b1 - dist txy b1)

dist2 (a,b) (c,d) = (a-c)^2 + (b-d)^2

dist x y = intSqrt $ dist2 x y

intSqrt :: Integer -> Double
intSqrt = sqrt.fromIntegral

readInt bs = case B.readInt bs of Just (n,_) -> fromIntegral n

parse (x:y:xys) = (x,y):parse xys
parse _ = []