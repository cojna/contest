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
    !n <- readLn
    ws <- L.unfoldr (runStateT parseInt) <$> B.getLine
    print $ solve n ws

solve :: Int -> [Int] -> Int
solve n ws = length $ L.foldl' step [] ws
  where
    step ms w = force $ go 0 ms
      where
        !th = 6 * w
        go !prev (m:ms)
            | prev <= th = min m (prev + w) : go m ms
            | otherwise = m : go m ms
        go !prev []
            | prev <= th = [prev + w]
            | otherwise = []

-------------------------------------------------------------------------------

lowerBoundM :: (Integral i, Monad m) => i -> i -> (i -> m Bool) -> m i
lowerBoundM low high p = go low high
  where
    go !low !high
        | high <= low = return high
        | otherwise = do
            pmid <- p mid
            if pmid
            then go low mid
            else go (mid + 1) high
      where
        mid = (low + high) `quot` 2
{-# INLINE lowerBoundM #-}

upperBoundM :: (Integral i, Monad m) => i -> i -> (i -> m Bool) -> m i
upperBoundM low high p = do
    phigh <- p high
    if phigh
    then return high
    else subtract 1 <$> lowerBoundM low high (fmap not.p)
{-# INLINE upperBoundM #-}

type Parser a = StateT B.ByteString Maybe a

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
