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
    n <- readLn
    xs <- unfoldr (B.readInteger.B.dropWhile isSpace) <$> B.getLine
    putStrLn.unwords.map show $ solve n xs

solve :: Int -> [Integer] -> [Integer]
solve n xs = head [hist | x<-xs, Just hist <- pure $ dfs x [x] (S.delete x $ S.fromList xs)]
  where
    dfs _ hist unused | S.null unused = Just $ reverse hist
    dfs cur hist unused = case quotRem cur 3 of
        (q, 0)
            | S.member q unused && S.member c2 unused ->
                dfs q (q:hist) (S.delete q unused) <|> dfs c2 (c2:hist) (S.delete c2 unused)
            | S.member q unused -> dfs q (q:hist) (S.delete q unused)
          where
            !c2 = 2 * cur
        _
            | S.member c2 unused -> dfs c2 (c2:hist) (S.delete c2 unused)
            | otherwise -> Nothing
          where
            !c2 = 2 * cur

