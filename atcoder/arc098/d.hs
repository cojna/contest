{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
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
    xs <- unfoldr (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve n xs

solve :: Int -> [Int] ->  Int
solve n xs = go 0 (Q 0 0 [] []) xs
  where
    go !res q (x:xs)
        | Just q' <- enqueue x q = go res q' xs
    go res q xs
        | Just q' <- snd <$> dequeue q = go (res + size q) q' xs
        | otherwise = res

data Q = Q !Int !Int [Int] [Int] deriving Show

size :: Q -> Int
size (Q _ l _ _) = l

enqueue :: Int -> Q -> Maybe Q
enqueue x (Q set l fs rs)
    | x .&. set == 0 = Just $ Q (set .|. x) (l + 1) fs (x:rs)
    | otherwise = Nothing

dequeue :: Q -> Maybe (Int, Q)
dequeue (Q _ 0 _ _)     = Nothing
dequeue (Q set l [] rs) = dequeue (Q set l (reverse rs) [])
dequeue (Q set l (f:fs) rs) = case Q (set `xor` f) (l - 1) fs rs of
    q -> Just (f, q)
