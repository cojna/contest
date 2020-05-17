{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances       #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards               #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeApplications         #-}
{-# LANGUAGE TypeFamilies, ViewPatterns                                   #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString             as B
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Internal    as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import qualified Data.Foldable               as F
import           Data.Function
import           Data.Functor.Identity
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import           Data.Ratio
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import           System.Environment
import           System.IO
import           System.Process
import           Unsafe.Coerce

main :: IO ()
main = withGCJ $ do
    [l, r] <- map read.words <$> getLine :: IO [Integer]
    case solve l r of
        (x, y, z) -> putStrLn . unwords . map show $ [x, y, z]

lim :: Integer
lim = 10 ^ 10

solve :: Integer -> Integer -> (Integer, Integer, Integer)
solve !l0 !r0 = case compare l0 r0 of
    LT -> solve' cnt0 l0 (r0 - f cnt0)
    EQ -> solve' 0 l0 r0
    GT -> solve' cnt0 (l0 - f cnt0) r0    
  where
    !cnt0 = calc0 l0 r0

solve' cnt0 l0 r0 = case compare l0 r0 of
    LT -> let cntL = calc (cnt0 + 2) l0
              cntR = calc (cnt0 + 1) r0
          in case compare cntL cntR of
            GT -> (cnt0 + 2 * cntR, l0 - g (cnt0 + 2) cntR, r0 - g (cnt0 + 1) cntR )
            EQ -> (cnt0 + 2 * cntR, l0 - g (cnt0 + 2) cntR, r0 - g (cnt0 + 1) cntR )
            LT -> (cnt0 + 2 * cntL + 1, l0 - g (cnt0 + 2) cntL, r0 - g (cnt0 + 1) (cntL + 1))
    _ -> let cntL = calc (cnt0 + 1) l0
             cntR = calc (cnt0 + 2) r0
         in case compare cntL cntR of
            LT -> (cnt0 + 2 * cntL, l0 - g (cnt0 + 1) cntL, r0 - g (cnt0 + 2) cntL )
            EQ -> (cnt0 + 2 * cntL, l0 - g (cnt0 + 1) cntL, r0 - g (cnt0 + 2) cntL )
            GT -> (cnt0 + 2 * cntR + 1, l0 - g (cnt0 + 1) (cntR + 1), r0 - g (cnt0 + 2) cntR)

calc0 l0 r0
     | l0 >= r0 = upperBound 0 lim $ \i ->
                l0 - f i >= r0
     | otherwise = upperBound 0 lim $ \i ->
                r0 - f i >= l0

calc base k = upperBound 0 lim $ \i -> g base i <= k

f :: Integer -> Integer
f n = n * (n + 1) `div` 2
g :: Integer -> Integer -> Integer
g base n = (base + (n - 1)) * n

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        h = toInteger high
        l = toInteger low
        mid = fromIntegral $ l + div (h - l) 2
{-# INLINE lowerBound #-}

upperBound :: (Integral i) => i -> i -> (i -> Bool) -> i
upperBound low high p
    | p high = high
    | otherwise = lowerBound low high (not.p) - 1
{-# INLINE upperBound #-}


-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}
rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}
type Parser a = StateT C.ByteString Maybe a
runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}
int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}
int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}
char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}
byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}
-------------------------------------------------------------------------------
-- Control.GCJ
-------------------------------------------------------------------------------
formatGCJ :: Int -> String
formatGCJ i = "Case #" <> shows i ": "

withGCJ :: IO () -> IO ()
withGCJ f = getArgs >>= \case
    ["--debug"] -> f
    [] -> do
        t <- readLn
        mapM_ ((*> f) . putStr . formatGCJ) [1..t]
    args -> error $ show args

