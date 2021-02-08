{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, LambdaCase, MagicHash                   #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings       #-}
{-# LANGUAGE RecordWildCards, TupleSections, TypeFamilies, ViewPatterns #-}

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
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, m, k] <- map read.words <$> getLine :: IO [Int]
    print $ solve n m k

#define MOD 1000000007
#define FACT_CACHE_SIZE 200100

solve :: Int -> Int -> Int -> IntMod
solve n m k = resX + resY
  where
    resX = sum [intMod ((n - d) * m * m) * IntMod d | d<-[1..n-1]]
        * comb (n * m - 2) (k - 2)
    resY = sum [intMod ((m - d) * n * n) * IntMod d | d<-[1..m-1]]
        * comb (n * m - 2) (k - 2)

-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
-------------------------------------------------------------------------------
-- Data.IntMod
-------------------------------------------------------------------------------
modulus :: (Num a) => a
modulus = MOD
{-# INLINE modulus #-}
infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%
(+%) :: Int -> Int -> Int
(I# x#) +% (I# y#) = case x# +# y# of { r# -> I# (r# -# ((r# >=# MOD#) *# MOD#))}
{-# INLINE (+%) #-}
(-%) :: Int -> Int -> Int
(I# x#) -% (I# y#) = case x# -# y# of { r# -> I# (r# +# ((r# <# 0#) *# MOD#))}
{-# INLINE (-%) #-}
(*%) :: Int -> Int -> Int
(I# x#) *% (I# y#) = I# ((x# *# y#) `remInt#` MOD#)
{-# INLINE (*%) #-}
(/%) :: Int -> Int -> Int
(I# x#) /% (I# y#) = go# y# MOD# 1# 0# where { go# a# b# u# v# | isTrue# (b# ># 0#) = case a# `quotInt#` b# of { q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))} | otherwise = I# ((x# *# (u# +# MOD#)) `remInt#` MOD#)}
{-# INLINE (/%) #-}
(^%) :: Int -> Int -> Int
x ^% n | n > 0 = go 1 x n | n == 0 = 1 | otherwise = go 1 (1 /% x) (-n) where { go !acc !y !m | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1) | m == 1 = acc *% y | otherwise = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)}
newtype IntMod = IntMod{getIntMod :: Int} deriving (Eq, Ord)
intMod :: (Integral a) => a -> IntMod
intMod x = fromIntegral $ mod (fromIntegral x) MOD
{-# INLINE intMod #-}
intModValidate :: IntMod -> Bool
intModValidate (IntMod x) = 0 <= x && x < MOD
{-# INLINE intModValidate #-}
instance Show IntMod where { show (IntMod x) = show x}
instance Bounded IntMod where { minBound = IntMod 0; maxBound = IntMod $ modulus - 1}
instance Enum IntMod where { toEnum = intMod; fromEnum = coerce}
instance Real IntMod where { toRational = coerce (toRational :: Int -> Rational)}
instance Integral IntMod where { quotRem x y = (x / y, x - x / y * y); toInteger = coerce (toInteger :: Int -> Integer)}
instance Num IntMod where { (+) = coerce (+%); (-) = coerce (-%); (*) = coerce (*%); abs = id; signum = const (IntMod 1); fromInteger x = (coerce :: Int -> IntMod) . fromInteger $ mod x modulus}
instance Fractional IntMod where { (/) = coerce (/%); fromRational q = fromInteger (numerator q) / fromInteger (denominator q)}
newtype instance  UM.MVector s IntMod = MV_IntMod (UM.MVector s Int)
newtype instance  U.Vector IntMod = V_IntMod (U.Vector Int)
instance U.Unbox IntMod
instance GM.MVector UM.MVector IntMod where { basicLength (MV_IntMod v) = GM.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (MV_IntMod v) = MV_IntMod $ GM.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicOverlaps (MV_IntMod v1) (MV_IntMod v2) = GM.basicOverlaps v1 v2; {-# INLINE basicOverlaps #-}; basicUnsafeNew n = MV_IntMod `liftM` GM.basicUnsafeNew n; {-# INLINE basicUnsafeNew #-}; basicInitialize (MV_IntMod v) = GM.basicInitialize v; {-# INLINE basicInitialize #-}; basicUnsafeReplicate n x = MV_IntMod `liftM` GM.basicUnsafeReplicate n (coerce x); {-# INLINE basicUnsafeReplicate #-}; basicUnsafeRead (MV_IntMod v) i = coerce `liftM` GM.basicUnsafeRead v i; {-# INLINE basicUnsafeRead #-}; basicUnsafeWrite (MV_IntMod v) i x = GM.basicUnsafeWrite v i (coerce x); {-# INLINE basicUnsafeWrite #-}; basicClear (MV_IntMod v) = GM.basicClear v; {-# INLINE basicClear #-}; basicSet (MV_IntMod v) x = GM.basicSet v (coerce x); {-# INLINE basicSet #-}; basicUnsafeCopy (MV_IntMod v1) (MV_IntMod v2) = GM.basicUnsafeCopy v1 v2; {-# INLINE basicUnsafeCopy #-}; basicUnsafeMove (MV_IntMod v1) (MV_IntMod v2) = GM.basicUnsafeMove v1 v2; {-# INLINE basicUnsafeMove #-}; basicUnsafeGrow (MV_IntMod v) n = MV_IntMod `liftM` GM.basicUnsafeGrow v n; {-# INLINE basicUnsafeGrow #-}}
instance G.Vector U.Vector IntMod where { basicUnsafeFreeze (MV_IntMod v) = V_IntMod `liftM` G.basicUnsafeFreeze v; {-# INLINE basicUnsafeFreeze #-}; basicUnsafeThaw (V_IntMod v) = MV_IntMod `liftM` G.basicUnsafeThaw v; {-# INLINE basicUnsafeThaw #-}; basicLength (V_IntMod v) = G.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (V_IntMod v) = V_IntMod $ G.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicUnsafeIndexM (V_IntMod v) i = coerce `liftM` G.basicUnsafeIndexM v i; {-# INLINE basicUnsafeIndexM #-}; basicUnsafeCopy (MV_IntMod mv) (V_IntMod v) = G.basicUnsafeCopy mv v; elemseq _ = seq; {-# INLINE elemseq #-}}
-------------------------------------------------------------------------------
-- Math.Combinatrics
-------------------------------------------------------------------------------
fact :: Int -> IntMod
fact = U.unsafeIndex factCache
{-# INLINE fact #-}
recipFact :: Int -> IntMod
recipFact = U.unsafeIndex recipFactCache
{-# INLINE recipFact #-}
comb :: Int -> Int -> IntMod
comb n k = fact n * recipFact (n - k) * recipFact k
{-# INLINE comb #-}
factCacheSize :: Int
factCacheSize = min (modulus - 1) FACT_CACHE_SIZE
{-# INLINE factCacheSize #-}
factCache :: U.Vector IntMod
factCache = U.scanl' (\ x y -> x * coerce y) (1 :: IntMod) $ U.generate factCacheSize (+ 1)
{-# NOINLINE factCache #-}
recipFactCache :: U.Vector IntMod
recipFactCache = U.scanr' ((*) . coerce) (1 / factCache U.! factCacheSize) $ U.generate factCacheSize (+ 1)
{-# NOINLINE recipFactCache #-}
