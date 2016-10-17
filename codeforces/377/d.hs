{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
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

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    ds <- map readInt.B.words <$> B.getLine
    xs <- map (fromIntegral.readInt).B.words <$> B.getLine
    print $ solve n m ds xs

solve :: Int -> Int -> [Int] -> [Int64] -> Int
solve n m ds xs
  | isOK n = lowerBound 1 n isOK
  | otherwise = -1
  where
    arr :: UArray Int Int64
    arr = listArray (1, m) xs
    isOK i = go (IS.fromList[1..m]) 0 . reverse $ take i ds
      where
        go !unused !acc (x:xs)
          | IS.member x unused = go (IS.delete x unused) (acc+arr!x) xs
          | otherwise = go unused (max 0 $ acc-1) xs
        go unused acc [] = IS.null unused && acc == 0

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p =  go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        mid = (low + high) `quot` 2
{-# INLINE lowerBound #-}
