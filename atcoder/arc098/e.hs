{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           Unsafe.Coerce

main :: IO ()
main = do
    [n,k,q] <- map read.words <$> getLine :: IO [Int]
    xs <- unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve n k q xs

solve :: Int -> Int -> Int -> [Int] -> Int
solve n k q xs = minimum $ do
    l <- xs
    let removed = step l
    guard $ length removed == q
    return $! maximum removed - l
  where
    step l = take q . sort . concatMap take' $ splitWhen (<l) xs
    take' xs = take (length xs - k + 1) $ sort xs
