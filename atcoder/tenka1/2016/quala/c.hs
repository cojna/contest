{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
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

main = do
  n <- readLn
  xys<- fmap (filter (\(xs,ys)-> not $ xs `isPrefixOf` ys )).replicateM n $ do
      [xs, ys] <- words <$> getLine
      return $ diff xs ys
  if any (\(xs, ys)->ys `isPrefixOf` xs) xys
  then print (-1)
  else do
    case solve (map convert xys) of
        Just cs -> putStrLn cs
        Nothing -> print (-1)

solve :: [(Char, Char)] -> Maybe String
solve pairs = go ['a'..'z'] pairs
  where
    go rest [] = Just rest
    go [] pairs = Just []
    go rest pairs
      | (c':_) <- [c | c<-rest, all((/=c).snd)pairs]
        , Just res<-go (delete c' rest) (filter ((/= c').fst) pairs) = Just $ c' : res
      | otherwise = Nothing

convert ([x],[y]) = (x, y)
diff (x:xs) (y:ys)
  | x == y = diff xs ys
  | otherwise = ([x], [y])
diff xs ys = (xs, ys)





























