{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, DerivingStrategies  #-}
{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, LambdaCase #-}
{-# LANGUAGE MagicHash, MultiParamTypeClasses, MultiWayIf           #-}
{-# LANGUAGE NumericUnderscores, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RankNTypes, RecordWildCards, ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeApplications    #-}
{-# LANGUAGE TypeFamilies, TypeInType, UnboxedTuples, ViewPatterns  #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Bool
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Char8             as C
import qualified Data.ByteString.Internal          as B
import qualified Data.ByteString.Unsafe            as B
import           Data.Char
import qualified Data.Foldable                     as F
import           Data.Function
import           Data.Functor.Identity
import qualified Data.IntMap.Strict                as IM
import qualified Data.IntSet                       as IS
import qualified Data.List                         as L
import qualified Data.Map.Strict                   as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                          as S
import           Data.Tuple
import qualified Data.Vector                       as V
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import           Data.Vector.Fusion.Util
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import qualified Data.Vector.Mutable               as VM
import qualified Data.Vector.Primitive             as P
import qualified Data.Vector.Primitive.Mutable     as PM
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM
import           Debug.Trace
import           Foreign                           hiding (void)
import           GHC.Exts
import           GHC.TypeLits
import qualified System.IO                         as IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    n <- readLn :: IO Int
    (vws,qs) <- U.splitAt (2*n) . U.unfoldrN (2*n+200001) (runParser int) <$> C.getContents
    let q = U.head qs
    B.hPutBuilder IO.stdout
        . U.foldr (\x b -> B.intDec x <> (B.char7 '\n' <> b)) mempty
        $ solve n (tuples2N n vws) q (tuples2N q $ U.slice 1 (2 * q) qs)

wSize :: Int
wSize = 100001
ix :: Int -> Int -> Int
ix i j = i * wSize + j
{-# INLINE ix #-}

cacheSize, cacheWidth :: Int
cacheSize = 0x400
cacheWidth = 10

solve :: Int -> U.Vector Int -> Int -> U.Vector Int -> U.Vector Int
solve n0 (U.cons 0 -> !tr) q qs = U.map (uncurry query.decode) qs
  where
    !table = U.create $ do
        dp <- UM.unsafeNew (cacheSize * wSize)
        UM.set (UM.take wSize dp) 0
        flip U.imapM_ (U.take cacheSize tr) $ \i vw -> when (i > 0) $ do
            let (!v, !w) = decode vw
            let !p = unsafeShiftRL i 1
            UM.unsafeCopy
                (UM.unsafeSlice (ix i 0) wSize dp)
                (UM.unsafeSlice (ix p 0) wSize dp)
            flip MS.mapM_ (MS.generate (wSize - w) id) $ \j -> do
                v' <- (+v) <$!> UM.unsafeRead dp (ix p j)
                UM.unsafeModify dp (max v') (ix i (j + w))
        return dp

    query v l
        | v < cacheSize = table U.! ix v l
        | otherwise =
            let !pathSize = (64 - cacheWidth) - countLeadingZeros v
                !base = ix (unsafeShiftR v pathSize) l
            in unId
                . MS.foldl' max 0
                . MS.map (\encoded ->
                    let (score, weight) = decode encoded
                    in score + U.unsafeIndex table (base - weight))
                . MS.filter ((<= l) . getWeight)
                $ MS.generateM (shiftL 1 pathSize) $ \set ->
                    MS.foldl' (+) 0
                        . MS.map (U.unsafeIndex tr . unsafeShiftR v)
                        . MS.filter ((== 1) . (.&. 1) . unsafeShiftR set)
                        $ MS.generate pathSize id

encode :: Int -> Int -> Int
encode x y = unsafeShiftL x 32 + y
{-# INLINE encode #-}

decode :: Int -> (Int, Int)
decode xy = (x, y)
  where
    !x = unsafeShiftRL xy 32 .&. 0xffff_ffff
    !y = xy .&. 0xffff_ffff
{-# INLINE decode #-}

getScore :: Int -> Int
getScore xy = unsafeShiftRL xy 32 .&. 0xffff_ffff
{-# INLINE getScore #-}

getWeight :: Int -> Int
getWeight xy = xy .&. 0xffff_ffff
{-# INLINE getWeight #-}


tuples2N :: Int -> U.Vector Int -> U.Vector Int
tuples2N n = U.unfoldrN n $ \v ->
    let !x = U.unsafeIndex v 0
        !y = U.unsafeIndex v 1
        !e = encode x y
    in Just (e, U.unsafeDrop 2 v)

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

