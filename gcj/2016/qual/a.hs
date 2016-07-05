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
solve 0 = Nothing
solve n = Just . go ['0'..'9'] $ map (*n) [1..]
  where
    go unused (x:xs) = case unused \\ show x of
        [] -> x
        ys -> go ys xs

type Task = Integer
getTask :: IO Task
getTask = readLn

type Answer = Maybe Integer
putAnswer :: Answer -> IO ()
putAnswer = maybe (putStrLn "INSOMNIA") print

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
