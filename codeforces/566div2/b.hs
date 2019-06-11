{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
#ifndef DEBUG
{-# LANGUAGE Safe              #-}
#endif

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Array.Unboxed
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import qualified Data.List                  as L
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                   as S
import           Data.Tuple
import           Data.Word
#ifdef DEBUG
import           Debug.Trace
#endif
import Foreign

main :: IO ()
main = do
  [h, w]<- L.unfoldr (C.readInt.C.dropWhile isSpace) <$> C.getLine
  cs <- filter (not.isSpace) <$> getContents
  putStrLn.bool"NO""YES" $ solve h w cs

solve :: Int -> Int -> String -> Bool
solve _ _ cs | all (== '.') cs = False
solve h w cs = minx < x && x < maxx && miny < y && y < maxy && set == cross
  where
    xys = [quotRem ij w|(ij, '*')<-zip[0..]cs]
    !set = S.fromList xys
    !minx = minimum $ map fst xys
    !maxx = maximum $ map fst xys
    !miny = minimum $ map snd xys
    !maxy = maximum $ map snd xys
    !x = fst . head $ filter ((==miny).snd) xys
    !y = snd . head $ filter ((==minx).fst) xys
    cross = S.fromList $ map ((,) x) [miny..maxy] ++ map (flip (,) y) [minx..maxx]
