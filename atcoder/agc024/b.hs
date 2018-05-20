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
    n <- readLn :: IO Int
    xs <- L.unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getContents
    print $ solve n xs

solve :: Int -> [Int] -> Int
solve n xs = go IM.empty xs
  where
    go :: IM.IntMap Int -> [Int] -> Int
    go !m (x:xs) = case IM.lookup (x - 1) m of
        Just len -> go (IM.insert x (len + 1) $ IM.delete (x - 1) m) xs
        Nothing  -> go (IM.insert x 1 m) xs
    go m [] = (n - ). maximum.map snd $ IM.toAscList m

