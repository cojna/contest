{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
    !n <- readLn :: IO Int
    bs <- B.filter (\c -> 'a' <= c && c <= 'z') <$> B.getContents
    print $ solve n bs

solve :: Int -> B.ByteString -> Int
solve n bs = sum [n | a<-[0..n-1], validateA a]
  where
    idx x y = x * n + y
    validateA :: Int -> Bool
    validateA a = and $ do
        i <- [0..n-1]
        j <- [i..n-1]
        return $! B.unsafeIndex bs (idx ((a + i) `rem` n) j) ==
            B.unsafeIndex bs (idx ((a + j) `rem` n) i)
