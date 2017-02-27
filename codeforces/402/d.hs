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
  t <- getLine
  p <- getLine
  ps <- map readInt.B.words <$> B.getLine
  print $ solve t p ps

solve :: String -> String -> [Int] -> Int
solve t p ps = upperBound predicate 0 (length t)
  where
    predicate i = go 1 t p
      where
        !invalid = IS.fromList $ take i ps
        go !i (x:xs) (y:ys)
          | x == y && IS.notMember i invalid = go (i+1) xs ys
          | otherwise = go (i+1) xs (y:ys)
        go _ _ [] = True
        go _ _ _ = False


upperBound :: (Integral i) => (i -> Bool) -> i -> i -> i
upperBound p low high = go low high
   where
     go !low !high
       | high <= low = low
       | p mid       = go mid high
       | otherwise   = go low (mid - 1)
      where
        mid = (low + high + 1) `quot` 2
{-# INLINE upperBound #-}
