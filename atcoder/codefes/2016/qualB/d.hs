{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import qualified Data.ByteString.Char8 as B

readInt bs = case B.readInt bs of Just (n, _) -> n

main :: IO ()
main = do
    (_:xs) <-  map readInt.B.words <$> B.getContents
    print $ solve xs

solve :: [Int] -> Int
solve (x:xs) = go (x-1) 1 xs
 where
  go !res !m (x:xs)
    | x <= m = go res m xs
    | x == m + 1 = go res x xs
    | (q, 0) <- divMod x (m+1), q > 0 = go (res+q-1) m xs
    | otherwise = go (div x (m+1) + res) m xs
  go res _ [] = res
