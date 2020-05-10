{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, DerivingStrategies      #-}
{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ImplicitParams, KindSignatures #-}
{-# LANGUAGE LambdaCase, MagicHash, MultiParamTypeClasses, MultiWayIf   #-}
{-# LANGUAGE NumericUnderscores, OverloadedStrings, PatternSynonyms     #-}
{-# LANGUAGE RankNTypes, RecordWildCards, ScopedTypeVariables           #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeApplications        #-}
{-# LANGUAGE TypeFamilies, TypeInType, UnboxedTuples, ViewPatterns      #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Unsafe        as B
import           Data.Char
import qualified Data.Foldable                 as F
import           Data.Function
import           Data.Functor.Identity
import qualified Data.IntMap.Strict            as IM
import qualified Data.IntSet                   as IS
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                      as S
import           Data.Tuple
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as GM
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Primitive         as P
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector.Unboxed.Mutable   as UM
import           Debug.Trace
import           Foreign                       hiding (void)
import           GHC.Exts
import qualified System.IO                     as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    n <- readLn :: IO Int
    xs <- U.fromListN n . map encode . lines <$> getContents
    putStrLn.bool"No""Yes" $ solve n xs

encode :: String -> Word64
encode cs = go 0 0 cs
  where
    go !m !acc ('(':cs) = go m (acc + 1) cs
    go !m !acc (')':cs) = go (min m (acc - 1)) (acc - 1) cs
    go !m !acc [] = unsafeCoerce @Int @Word64 $ unsafeShiftL (-m) 32 .|. (max 0 $ acc - m)

decode :: Word64 -> (Int, Int)
decode xy = (x, y)
  where
    !x = unsafeCoerce @Word64 @Int $ unsafeShiftR xy 32 .&. 0xffffffff
    !y = unsafeCoerce @Word64 @Int $ xy .&. 0xffffffff

solve :: Int -> U.Vector Word64 -> Bool
solve 1 encoded = U.head encoded == 0
solve n (U.filter (/=0) -> encoded) = U.null encoded || goL 0 (U.toList ls)
  where
    !ls = radixSort64
        $ U.filter (uncurry (<) . decode) encoded
    !rs = U.reverse
        . radixSort64
        . U.map (enc.swap.decode)
        $ U.filter (uncurry (>=) . decode) encoded
    enc (x, y) = unsafeCoerce $ unsafeShiftL x 32 .|. y
    goL !acc ((decode -> (x, y)):xs)
        | acc >= x = goL (acc - x + y) xs
        | otherwise = False
    goL !acc [] = goR acc (U.toList rs)

    goR !acc ((decode -> (y, x)):xs)
        | acc >= x = goR (acc - x + y) xs
        | otherwise = False
    goR !acc [] = acc == 0

radixSort64 :: U.Vector Word64 -> U.Vector Word64
radixSort64 v = F.foldl' step v [0, 16, 32, 48]
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step v k = U.create $ do
        pref <- U.unsafeThaw
            . U.prescanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate 0x10000 0)
            $ U.map (flip (,) 1 . mask k) v
        res <- UM.unsafeNew $ U.length v
        U.forM_ v $ \x -> do
            let !masked = mask k x
            i <- UM.unsafeRead pref masked
            UM.unsafeWrite pref masked $ i + 1
            UM.unsafeWrite res i x
        return res
{-# INLINE radixSort64 #-}

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

