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

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- U.unfoldrN n (B.readInt.B.dropWhile isSpace) <$> B.getLine
    case solve . U.toList $ U.reverse xs of
        Just (x, y) -> putStrLn.unwords.map show $ [x, y]
        Nothing -> print $ -1

solve :: [Int] -> Maybe (Int, Int)
solve (2:xs) = go 1 1 2 xs
  where
    go gl gr p (x:xs)
        | gl' <= gr'= go gl' gr' x xs
        | otherwise = Nothing
      where
        gll = gl * p
        glr = gl * p + p - 1
        gl' = div (gll + x - 1) x

        grl = gr * p
        grr = gr * p + p - 1
        gr' = div grr x
    go gl gr p [] = Just (p*gl, p*gr+p-1)
solve _ = Nothing

