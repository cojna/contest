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
main = getLine >> B.getContents >>= print.solve.B.words

solve :: [B.ByteString] -> Int
solve bss = go 0 1 [] IS.empty bss
  where
    go !res !num (top:stock) set ("remove":rest)
      | top == num = go res (num + 1) stock set rest
      | set' <- foldl' (flip IS.insert) set (top:stock) =
           go (res + 1) (num + 1) [] (IS.deleteMin set') rest
    go !res !num [] set ("remove":rest) =
        go res (num + 1) [] (IS.deleteMin set) rest
    go res num stock set ("add":x:rest) =
        go res num (readInt x:stock) set rest
    go res _ _ _ [] = res
