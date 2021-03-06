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
    x <- readLn
    putStrLn.unwords.map show $ solve x

solve :: Int -> [Int]
solve x = go $ IM.toList table
  where
    go ((k,a):ks) = case IM.lookup (abs $ k - x) table of
        Nothing -> go ks
        Just b
            | a^5-b^5==x -> [a, b]
            | otherwise -> [a, -b]

lim = 1000

table :: IM.IntMap Int
table = IM.fromList[(i^5, i)|i<-[0..lim]]

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

