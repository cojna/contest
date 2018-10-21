import           Control.Monad
import           Data.Bool

main :: IO ()
main = do
  q <- readLn
  replicateM_ q $ do
    [a, b, c] <- map read.words <$> getLine
    putStrLn . bool "No" "Yes" $ solve a b c

solve :: Int -> Int -> Int -> Bool
solve a b c
  | odd s = False
  | otherwise = qa * 100 + qb * 10 + qc == s2
  where
    s = 100 * a + 10 * b + c
    s2 = div s 2
    qa = min a $ div s2 100
    ra = s2 - 100 * qa
    qb = min b $ div ra 10
    rb = ra - 10 * qb
    qc = min c rb
