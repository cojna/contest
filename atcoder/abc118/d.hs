{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
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
import qualified Data.Foldable               as F
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

#define NOTHING (-1)

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    digits <- V.unfoldrN m (B.readInt.B.dropWhile isSpace) <$> B.getLine
    print $ solve n digits

solve :: Int -> V.Vector Int -> Integer
solve n digits = memoFix (n + 1) `flip` n $ \memo ->
    \case
        0 -> 0
        i -> V.maximum . flip V.map digits $ \digit ->
            if
                | cost U.! digit <= i -> case memo (i - cost U.! digit) of
                    NOTHING -> NOTHING
                    x       -> 10 * x + fromIntegral digit
                | otherwise -> NOTHING

cost :: U.Vector Int
cost = U.fromList [6, 2, 5, 5, 4, 5, 6, 3, 7, 6]

memoFix :: Int -> ((Int -> a) -> (Int -> a)) -> Int -> a
memoFix size f = (memo V.!)
  where
    memo = V.generate size (f (memo V.!))
