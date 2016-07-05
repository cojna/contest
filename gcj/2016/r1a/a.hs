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
solve cs = go [] cs
  where
    go [] (c:cs) = go [c] cs
    go res@(l:_) (c:cs)
      | c >= l = go (c:res) cs
      | otherwise = go (res++[c]) cs
    go res [] = res

type Task = String
getTask :: IO Task
getTask = getLine

type Answer = String
putAnswer :: Answer -> IO ()
putAnswer = putStrLn

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
