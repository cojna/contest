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
main = getLine >>= putStrLn.unwords.map show.solve.map read.words

solve :: [Int64] -> [Int64]
solve [n, a, b] = case [(x,quot(n-a*x)b)|x<-[0..div n a],rem(n - a * x)b==0] of
    []        -> [-1]
    ((x,y):_) -> gen a x ++ map(+(a*x))(gen b y)

gen :: Int64 -> Int64 -> [Int64]
gen a x = concat[map (\r->a*q+r+1) $ (a-1):[0..a-2] |q<-[0..x-1]]
