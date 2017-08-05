{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Data.Array.Unboxed
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict    as IM
import qualified Data.IntSet           as IS
import           Data.List
import qualified Data.Map.Strict       as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set              as S
import           Data.Tuple
import           Data.Word

readInt :: B.ByteString -> Int
readInt bs=case B.readInt bs of{Just(n,_)->n;_->error$"readInt error : bs = "++show bs;}
readInteger :: B.ByteString -> Integer
readInteger bs=case B.readInteger bs of{Just(n,_)->n;_->error$"readInteger error : bs = "++show bs;}

main :: IO ()
main = do
    k <- readLn
    ns <- map digitToInt.B.unpack.B.filter(\c->'0'<=c&&c<='9') <$> B.getLine
    print $ solve k ns

solve :: Int -> [Int] -> Int
solve k ns = go 0 (k - s) $ sort ns
  where
    !s = sum ns
    go !res !rest (x:xs)
      | rest <= 0 = res
      | otherwise = go (res + 1) (rest - (9 - x)) xs
    go res _ _ = res


