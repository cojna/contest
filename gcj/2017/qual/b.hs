{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Char
import           Data.Fixed
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace

main :: IO ()
main = runGCJ $ do
    n <- readLn
    print $ solveLarge n

solveSmall :: Int -> Int
solveSmall n
  | n > 1000 = 99999999999999999
  | otherwise = last $ filter (isSorted.show) [1..n]

solveLarge :: Int -> Int
solveLarge n = go "" '0' $ show n
  where
    go !acc prev (c:cs)
      | prev <= c = go (c:acc) c cs
      | otherwise = read $ (reverse . f $ c:acc) ++ (cs >> "9")
    go acc _ [] = read $ reverse acc

    f (x:y:xs)
     | x >= y = x:y:xs
     | otherwise = '9' : f (pred y:xs)
    f xs = xs


isSorted :: String ->  Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)


runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        traceIO $ shows i "/" ++ shows t ": "
        time <- flip (-) <$> getPOSIXTime <* main_ <*> getPOSIXTime
        traceIO $ (shows.msec) time "ms\n"

msec :: NominalDiffTime -> Int
msec s = let t = realToFrac s :: Milli in fromEnum t
