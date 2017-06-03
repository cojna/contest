{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
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
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           Unsafe.Coerce

main :: IO ()
main = runGCJ $ do
    [_, p] <- map read.words <$> getLine :: IO [Int]
    xs <- map read.words <$> getLine :: IO [Int]
    print $ solve p xs

solve 2 xs = solve2 xs
solve 3 xs = solve3 xs
solve 4 xs = solve4 xs

solve2 xs = zero + div (one + 1) 2
  where
    ys = map (`mod` 2) xs
    zero = length $ filter (==0) ys
    one = length $ filter (==1) ys

solve3 xs = zero + min one two + div (rest + 2) 3
  where
    ys = map (`mod` 3) xs
    zero = length $ filter (==0) ys
    one = length $ filter (==1) ys
    two = length $ filter (==2) ys
    rest = max one two - min one two

solve4 xs = zero + min one three + div two 2 + rest
  where
    ys = map (`mod` 4) xs
    zero = length $ filter (==0) ys
    one = length $ filter (==1) ys
    two = length $ filter (==2) ys
    three = length $ filter (==3) ys
    rest1 = max 0 $ one - three
    rest3 = max 0 $ three - one
    rest2 = two `mod` 2
    rest
      | rest1 > 0, rest2 == 1 = 1 + div (max 0 (rest1 - 2) + 3) 4
      | rest3 > 0, rest2 == 1 = 1 + div (max 0 (rest3 - 2) + 3) 4
      | rest2 == 1 = 1
      | rest1 > 0 = div (rest1 + 3) 4
      | rest3 > 0 = div (rest3 + 3) 4
      | otherwise = 0

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
