{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Int
import qualified Data.List                        as L

main :: IO ()
main = do
    [n, _m] <- map read.words <$> getLine
    xds <- map (fromIntegral *** fromIntegral).L.unfoldr (runStateT parseInt2) <$> B.getContents
    print $ solve n xds

solve :: Int64 -> [(Int64, Int64)] -> Double
solve n xds = (fromIntegral . sum $ map diff ds) / fromIntegral n + fromIntegral (sum xs)
  where
    !up = sum[1..n-1]
    !down | odd n = 2 * sum[1..div n 2]
          | otherwise = 2 * sum[1..div n 2] - div n 2

    (xs, ds) = unzip xds
    diff d
        | d > 0 = d * up
        | d == 0 = 0
        | otherwise = d * down


type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = StateT $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = (,,) <$> parseInt <*> parseInt <*> parseInt
