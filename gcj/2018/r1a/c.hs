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
main = runGCJ $ do
    [n, p] <- map read.words <$> getLine
    whs <- replicateM n $ do
        [w, h] <- map read.words <$> getLine
        return $!! (min h w, max h w)
    print $ solve p whs

eps :: Double
eps = 1e-8

solve :: Int -> [(Int, Int)] -> Double
solve p_ whs = fromIntegral offset + search p 0.0 allRanges
  where
    p :: Double
    !p = fromIntegral $ p_ - offset
    !offset = sum $ map ((*2).uncurry(+)) whs
    mkRange (x, y) = R l r
      where
        l = fromIntegral $ 2 * x
        r = 2.0 * sqrt (fromIntegral x^2 + fromIntegral y^2)
    allRanges = go [] $ map mkRange whs
    go set (r:rs) = go (L.insert r $ concatMap (addRange r) set) rs
    go set []     = set
    search !best !res (R l r:rest)
      | l <= p, p <= r = p
      | r <= p, p - r < best = search (p - r) r rest
      | otherwise = search best res rest
    search _ res [] = res

data Range = R !Double !Double deriving (Eq, Ord, Show)

addRange :: Range -> Range -> [Range]
addRange (R lk rk) (R lx rx)
    | lk + lx <= rx = [R lx (rx + rk)]
    | otherwise = [R lx rx, R (lx + lk) (rx + rk)]

-------------------------------------------------------------------------------

type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = coerce $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

rep, rev :: Applicative f => Int -> (Int -> f ()) -> f ()
rep n f=F.traverse_ f[0..n-1]
rev n f=F.traverse_(f.negate)[1-n..0]
for :: Applicative f => Int -> Int -> (Int -> f ()) -> f ()
for a b f=F.traverse_ f[a..b]
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f=readArray a i>>=writeArray a i.f
{-# INLINE modifyArray #-}
unsafeModify :: (MArray a e m, Ix i) => a i e -> Int -> (e -> e) -> m ()
unsafeModify a i f=unsafeRead a i>>=unsafeWrite a i.f
{-# INLINE unsafeModify #-}

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    !t <- readLn :: IO Int
    F.for_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_
