{-# LANGUAGE BangPatterns #-}
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Int
import qualified Data.List             as L

main :: IO ()
main = do
    [_, m] <- map read.words <$> getLine
    xs <- map fromIntegral.L.unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    putStrLn.unwords.map show $ solve m xs

solve :: Int64 -> [Int64] -> [Int64]
solve m xs0 = go 0 xs0
  where
    go !acc (x:xs)
        | acc + x < m = 0 : go (acc + x) xs
        | (q, r) <- quotRem (x - (m - acc)) m = (q + 1) : go r xs
    go _ [] = []
