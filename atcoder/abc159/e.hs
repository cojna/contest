{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances, KindSignatures, LambdaCase, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections      #-}
{-# LANGUAGE TypeFamilies, ViewPatterns                               #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
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
import           Data.Primitive
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
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [h, w, k] <- map read.words <$> getLine :: IO [Int]
    vs <- V.replicateM h $ U.unfoldrN w (runParser $ digitToInt <$> char) <$> C.getLine
    print $ solve h w k vs

solve :: Int -> Int -> Int -> V.Vector (U.Vector Int) -> Int
solve h w k vs = minimum [cost s | s <- [0..shiftL 1 h - 1]]
  where
    cost :: Word -> Int
    cost s
        | any (U.any (>k)) hs = maxBound
        | otherwise = fst
            . V.foldl' stepW (popCount s, U.replicate l 0)
            $ V.generate w (\i -> U.generate l (\j -> hs V.! j U.! i))
      where
        g :: [Int]
        !g = tail $ L.scanl' (+) 0 [bool 0 1 $ testBit s i | i<-[0..h-1]]
        !hs = V.generate (popCount s + 1) $ \i ->
            L.foldl' (U.zipWith (+)) (U.replicate w 0)
                . map ((vs V.!).snd)
                . filter ((==i).fst)
                $ zip g [0..]
        !l = V.length hs

    stepW (!cnt, !acc) col
        | U.maximum s <= k = (cnt, s)
        | otherwise = (cnt + 1, col)
      where
        !s = U.zipWith (+) acc col


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
