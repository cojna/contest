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
    [r,b,c] <- map read.words <$> getLine
    msps <- replicateM c $ do
        evalParser parseInt3 <$> B.getLine
    print $ solve r b msps

solve :: Int -> Int -> [(Int, Int, Int)] -> Int
solve r b msps = lowerBound 0 0x3f3f3f3f3f3f3f3f $ canFinish r b msps

canFinish :: Int -> Int -> [(Int, Int, Int)] -> Int -> Bool
canFinish r b msps t = sum cap >= b
  where
    cap = take r . L.sortBy (flip compare) . filter(>0) $ map limit msps
    limit (m, s, p) = min m $ max 0 (t - p) `quot` s

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = assert (low >= 0 && p high) $ go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        mid = low + (high - low) `quot` 2
{-# INLINE lowerBound #-}

-------------------------------------------------------------------------------

type Parser a = StateT B.ByteString Maybe a

runParser :: Parser a -> B.ByteString -> Maybe (a, B.ByteString)
runParser = coerce
{-# INLINE runParser #-}

evalParser :: Parser a -> B.ByteString -> a
evalParser p = maybe (error "parse error") id . evalStateT p

parseInt :: Parser Int
parseInt = coerce $ B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = (,,) <$> parseInt <*> parseInt <*> parseInt

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

