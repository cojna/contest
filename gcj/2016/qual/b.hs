{-# LANGUAGE BangPatterns #-}

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
solve cs = go 0 cs
  where
    go !acc [] = acc
    go !acc cs = case dropWhileEnd (=='+') cs of
        ('+':cs) -> go (acc + 2) . reverse . map f . dropWhile (=='-') . dropWhile (=='+') $ cs
        ('-':cs) -> go (acc + 1) . reverse . map f . dropWhile (=='-') $ cs
        _ -> acc
    f '+' = '-'
    f '-' = '+'
    f _ = undefined

type Task = String
getTask :: IO Task
getTask = getLine

type Answer = Int
putAnswer :: Answer -> IO ()
putAnswer = print

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
