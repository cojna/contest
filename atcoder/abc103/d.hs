{-# LANGUAGE BangPatterns #-}

import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Coerce
import qualified Data.List                        as L

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    pairs <- map (\(x,y)->(x-1,y-1)).L.unfoldr (runStateT parseInt2) <$> B.getContents
    print $ solve pairs

inf :: Int
inf = 0x3f3f3f3f

solve :: [(Int, Int)] -> Int
solve pairs = go 0 inf $ L.sort pairs
  where
    go !res minR ((l, r):lrs)
        | minR <= l = go (res + 1) inf ((l, r):lrs)
        | otherwise = go res (min minR r) lrs
    go res minR []
        | minR == inf = res
        | otherwise = res + 1

type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = coerce $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = (,,) <$> parseInt <*> parseInt <*> parseInt
