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
  !n <- readLn :: IO Integer
  if odd n
  then print 0
  else print $ (2 :: Integer) ^ div n 2
