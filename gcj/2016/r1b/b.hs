{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Control.Parallel.Strategies
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Fixed
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           System.IO
import           Unsafe.Coerce

data T = SAME | ZERO | NINE deriving Show

solve :: Task -> Answer
solve (xs0, ys0) = snd $ minimum [((evaluate xs ys,xs,ys),(xs,ys))|xs<-xss,ys<-yss]
  where
    xss = mapM (\x->if x=='?' then ['0'..'9'] else [x]) xs0
    yss = mapM (\x->if x=='?' then ['0'..'9'] else [x]) ys0
    evaluate xs ys = abs $ read xs - read ys

type Task = (String, String)
getTask :: IO Task
getTask = do
    [xs, ys] <- words <$> getLine
    return (xs, ys)

type Answer = (String, String)
putAnswer :: Answer -> IO ()
putAnswer (xs, ys)= putStrLn $ xs ++ " " ++ ys

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