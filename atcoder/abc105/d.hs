import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.List             as L

main :: IO ()
main = do
  [n, m] <- map read.words <$> getLine
  xs <- L.unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
  print $ solve m xs

solve :: Int -> [Int] -> Int
solve m xs = sum
    . map ((\l -> l * (l - 1) `quot` 2) . length)
    . L.group
    . L.sort
    $ scanl (\x y -> (x + y) `rem` m) 0 xs
