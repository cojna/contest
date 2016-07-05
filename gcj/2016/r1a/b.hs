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
solve (n,xss) = take n.map head.filter (odd.length).group . sort . concat $ xss

type Task = (Int,[[Int]])
getTask :: IO Task
getTask = do
    n <- readLn
    (,) n <$> replicateM (2*n-1) (map read.words <$> getLine)


type Answer = [Int]
putAnswer :: Answer -> IO ()
putAnswer = putStrLn.unwords.map show

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
