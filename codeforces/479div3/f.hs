{-# OPTIONS_GHC -O2 #-}
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

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    let res = solve n xs
    print $ length res
    putStrLn . unwords $ map show res

solve :: Int -> [Int] -> [Int]
solve _ xs = construct [1..] xs $ go IM.empty xs
  where
    go !im (x:xs) = case IM.lookup (x - 1) im of
        Nothing -> go (IM.insert x 1 im) xs
        Just l  -> go (IM.insertWith max x (l + 1) im) xs
    go im [] = snd $ maximum [(len, [lst-len+1..lst])|(lst, len)<-IM.toList im]
    construct (i:is) (x:xs) (y:ys)
        | x == y = i : construct is xs ys
        | otherwise = construct is xs (y:ys)
    construct _ _ _ = []
