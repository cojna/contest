{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
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
  [n,k] <- map readInt.B.words <$> B.getLine
  xs <- map readInt.B.words <$> B.getLine
  ys <- map readInt.B.words <$> B.getLine
  print $ solve k xs ys

solve :: Int -> [Int] -> [Int] -> Int64
solve k xs ys = sum[fromIntegral x | (d,x)<-zs0++zs2] + sum[fromIntegral $ x +d| (d,x)<-zs3]
  where
    diffs = reverse.sort $ zipWith(\x y->(y-x,x)) xs ys
    (zs0, zs1) = splitAt k diffs
    (zs2, zs3) = span ((>0).fst) zs1
