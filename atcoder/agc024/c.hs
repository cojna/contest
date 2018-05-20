{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State.Strict
import           Data.Array.Base
import           Data.Array.ST                    (STUArray, runSTUArray)
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Coerce
import qualified Data.Foldable                    as F
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict               as IM
import qualified Data.IntSet                      as IS
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                         as S
import           Data.STRef
import           Data.Tuple
import           Data.Word
import           Debug.Trace
import           GHC.Arr                          (Array, Ix (..), STArray)
import           GHC.Exts
import           System.Exit
import           System.IO

main :: IO ()
main = do
    !n <- readLn :: IO Int
    xs <- L.unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getContents
    print.maybe (-1) id $ solve n xs

solve n xs@(hd:_)
    | hd > 0 = Nothing
    | otherwise = go 0 0 n $ reverse xs
  where
    go !res !h !rest (0:xs)
        | h == 0 = go res 0 (rest - 1) xs
        | otherwise = Nothing
    go !res !h !rest (x:xs) = case compare h x of
        EQ -> go res (h - 1) (rest - 1) xs
        LT | x < rest -> go (res + x) (x - 1) (rest - 1) xs
        _  -> Nothing
    go res 0 0 [] = Just res
    go _ _ _ _ = Nothing

