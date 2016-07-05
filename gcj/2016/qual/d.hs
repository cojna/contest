import           Control.Applicative
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Fixed
import           Data.List
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           System.IO

solve :: Task -> Answer
solve (1, _, _) = Just [1]
solve (k, 1, s)
    | k <= s = Just [1..k]
    | otherwise = Nothing
solve (k, c, s)
    | k - 1 <= s = Just [2..k]
    | otherwise = Nothing

type Task = (Int, Int, Int)
getTask :: IO Task
getTask = do
    [k, c, s] <- map read.words <$> getLine
    return (k, c, s)

type Answer = Maybe [Int]
putAnswer :: Answer -> IO ()
putAnswer = putStrLn . maybe "IMPOSSIBLE" (unwords.map show)

main :: IO ()
main = do
  t <- readLn
  start <- getPOSIXTime
  answers <- parMap rdeepseq solve <$> replicateM t getTask
  foldM_ `flip` start `flip` (zip [1..] answers)$ \prev (i, answer) -> do
      putStr $ "Case #" ++ shows i ": "
      putAnswer answer
      cur <- getPOSIXTime
      hPutStr stderr $ shows i "/" ++ shows t ": "
      hPutStrLn stderr $ (shows.msec) (cur - prev) "ms"
      return cur

msec :: NominalDiffTime -> Int
msec s = let t = realToFrac s :: Milli in fromEnum t
