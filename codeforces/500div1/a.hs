{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Int
import qualified Data.List             as L

main :: IO ()
main = do
    n <- readLn
    xs <- map fromIntegral.L.unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve n xs

solve :: Int -> [Int64] -> Int64
solve 1 _ = 0
solve n xs = go res0 (tail left) [] right
  where
    !m = minimum xs
    !mm = maximum xs
    !res0 = (maximum left - minimum left) * (maximum right - minimum right)
    !sorted = L.sort xs
    (left, right) = splitAt n sorted
    go !res [] rs (x:xs)     = go res (reverse rs) [] (x:xs)
    go !res (f:fs) rs (x:xs) = go (min res $ (mm - m) * (x - f)) fs (x:rs) xs
    go res _ _ []            = res
