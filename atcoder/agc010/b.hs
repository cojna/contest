import           Control.Applicative
import           Data.Bool

main :: IO ()
main = do
  n <- readLn
  xs <- map read.words <$> getLine
  putStrLn.bool"NO""YES" $ solve n xs

solve :: Int -> [Int] -> Bool
solve n xs = r == 0 && map(flip rem n)ys == [rem(y+rem(i*m)n)n|i<-[0..n-1]]
  where
    s = n * (n + 1) `div` 2
    (m, r) = sum xs `divMod` s
    ys@(y:_) = map (subtract m) xs
