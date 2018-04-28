{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

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
    cs <- getLine
    putStrLn.maybe"-1"id $ solve cs

solve :: String -> Maybe String
solve cs
  | length cs < 26 = Just $ cs ++ take 1 [c | c<-['a'..], notElem c cs]
  | otherwise = runST $ do
      mv <- U.thaw $ U.fromList cs
      hasNext <- nextPermutation mv
      if hasNext
      then do
          ps <- U.toList <$> U.freeze mv
          return . Just . head . filter(>cs) $ inits ps
      else return Nothing

nothing :: Int
nothing = -1

nextPermutation :: (PrimMonad m, Ord a, GM.MVector v a) => v (PrimState m) a -> m Bool
nextPermutation v = do
    h <- GM.unsafeRead v 0
    (k, l) <- go nothing h nothing h 1
    if k /= nothing
    then do
        GM.unsafeSwap v k l
        GM.reverse $ GM.unsafeSlice (k+1) (n-k-1) v
        return True
    else return False
  where
    !n = GM.length v
    go !k !vk !l !prev !i
      | i < n = do
          vi <- GM.unsafeRead v i
          if prev < vi
          then go (i-1) prev i vi (i+1)
          else if vk < vi
          then go k vk i vi (i + 1)
          else go k vk l vi (i + 1)
      | otherwise = return (k, l)

