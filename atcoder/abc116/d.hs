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
    [n, k] <- map read.words <$> getLine
    tds <- U.unfoldrN n parseInt2 <$> B.getContents
    print $ solve n k tds

solve :: Int -> Int -> U.Vector (Int, Int) -> Int
solve n k tds = go res0 s0 size0 used0 (reverse $ buildQ xs) ys
  where
    (xs, ys) = splitAt k
        . sortBy (flip compare)
        . map swap
        $ U.toList tds
    !s0 = sum $ map fst xs
    !size0 = IS.size used0
    !used0 = IS.fromList $ map snd xs
    !res0 = s0 + size0 * size0

    go !res !s !size !used stack ((d, t):rest)
        | IS.member t used = go res s size used stack rest
        | otherwise = case stack of
            (top:stack')
                | size' <- size + 1
                , s' <- s - top + d
                , res' <- max res $ s' + size' * size'
                , used' <- IS.insert t used
                -> go res' s' size' used' stack' rest
            [] -> res
    go res _ _ _ _ [] = res

buildQ :: [(Int, Int)] -> [Int]
buildQ dts = go IS.empty dts
  where
    go !used ((d, t):rest)
      | IS.member t used = d : go used rest
      | otherwise = go (IS.insert t used) rest
    go _ [] = []

type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)