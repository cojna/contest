{-# LANGUAGE BangPatterns #-}

import           Control.Monad

main :: IO ()
main = do
  [n, m] <- map read.words <$> getLine
  xyrs <- replicateM n $ do
    [x, y, r] <- map read.words <$> getLine
    return (x, y, r)
  xys <- replicateM m $ do
    [x, y] <- map read.words <$> getLine
    return (x, y)
  print $ solve m xyrs xys

solve :: Int -> [(Double, Double, Double)] -> [(Double, Double)] -> Double
solve 0 xyrs _ = minR xyrs
solve _ xyrs xys = min (minR xyrs).upperBoundEPS 1e-8 0 1000 $ \r ->
    let circles = xyrs ++ [(x,y,r)|(x,y)<-xys]
    in and [ not $ hasIntersect c0 c1 | c0<-circles, c1<-circles, c0 /= c1]

minR xyrs = minimum $ 1000:[r | (_,_,r)<-xyrs]

type Circle = (Double, Double, Double)

hasIntersect :: Circle -> Circle -> Bool
hasIntersect (x0,y0,r0) (x1,y1,r1) = sqrt (dx * dx + dy * dy) + 1e-8 <= r0 + r1
  where
    dx = x1-x0
    dy = y1-y0

upperBoundEPS :: Double -> Double -> Double -> (Double -> Bool) -> Double
upperBoundEPS !eps low high p = go 50 low high
  where
    go !n !low !high
        | n == 0 || abs (high - low) < eps = low
        | p mid     = go (n - 1) mid high
        | otherwise = go (n - 1) low mid
      where
        mid = (low + high) * 0.5
