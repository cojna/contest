{-# OPTIONS_GHC -O2 #-}

import qualified Data.ByteString.Char8       as B
import           Data.Char
import qualified Data.Vector.Unboxed         as U

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- U.map (subtract 1).U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve n xs

solve :: Int -> U.Vector Int -> Int
solve _ xs = flip div 2 . U.length $ U.ifilter (\i x -> xs U.! x == i) xs

