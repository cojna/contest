{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Array.Unboxed
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8      as B
import           Data.Char
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                   as S
import           Data.Tuple
import           Data.Word

readInt :: B.ByteString -> Int
readInt bs=case B.readInt bs of{Just(n,_)->n;_->error$"readInt error : bs = "++show bs;}
readInteger :: B.ByteString -> Integer
readInteger bs=case B.readInteger bs of{Just(n,_)->n;_->error$"readInteger error : bs = "++show bs;}

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        mid = (low + high) `quot` 2

main :: IO ()
main = do
    [n,k,s,t] <- map read.words <$> getLine
    (cvs, gs) <- splitAt(2*n).map readInt.B.words <$> B.getContents
    print . maybe (-1) id $ solve s t (parse cvs) $ sort gs

parse (x:y:xys) = (x,y):parse xys
parse _ = []

solve :: Int -> Int -> [(Int, Int)] -> [Int] -> Maybe Int
solve s t cvs gs
  | isValid maxV = Just $ minimum . map fst $ filter ((>=needV).snd) cvs
  | otherwise = Nothing
  where
    maxV = maximum $ map snd cvs
    needV = lowerBound 0 maxV isValid
    isValid :: Int -> Bool
    isValid v = go 0 0 $ gs ++ [s]
      where
        go !cur !time (g:rest)
          | d > v = False
          | otherwise = go g (time + t') rest
          where
            d = g - cur
            r = min d $ v - d
            t' = 2 * (max 0 $ d - r) + r
        go _ !time [] = time <= t

