{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.Bool

main :: IO ()
main = do
    [txa, tya, txb, tyb, t, v] <- map read.words <$> getLine
    n <- readLn
    xys <- replicateM n $ do
        [!x, !y] <- map read.words <$> getLine
        return (x, y)
    putStrLn.bool"NO""YES" $ solve txa tya txb tyb t v xys

solve txa tya txb tyb t v xys = any p xys
  where
    dist (x0, y0) (x1, y1) = sqrt $ dx * dx + dy * dy
      where
        !dx = x1 - x0
        !dy = y1 - y0
    src = (txa, tya)
    dst = (txb, tyb)
    p xy = dist src xy + dist xy dst <= t * v