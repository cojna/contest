{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
#ifndef DEBUG
{-# LANGUAGE Safe              #-}
#endif
 
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Array                 as A
import qualified Data.Array.Unboxed         as UA
import qualified Data.Array.ST.Safe         as MA
import           Data.Bool
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import qualified Data.List                  as L
import qualified Data.List.NonEmpty         as NL
import           Data.Monoid
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import qualified Data.Set                   as S
import           Data.Tuple
import           Foreign
import qualified System.IO                  as IO
#ifdef DEBUG
import           Debug.Trace
#endif
 
main :: IO ()
main = do
    n <- readLn
    xs <- L.unfoldr (C.readInt.C.dropWhile isSpace) <$> C.getLine
    ys <- L.unfoldr (C.readInt.C.dropWhile isSpace) <$> C.getLine
    print $ solve n xs ys
 
solve :: Int -> [Int] -> [Int] -> Int
solve n xs ys = go IS.empty xs ys
  where
    go fined (x:xs) (y:ys)
        | x == y = go fined xs ys
        | IS.member x fined = go fined xs (y:ys)
        | otherwise = go (IS.insert y fined) (x:xs) ys
    go fined _ [] = IS.size fined