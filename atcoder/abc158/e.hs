{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, DataKinds, FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances, KindSignatures, LambdaCase, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings     #-}
{-# LANGUAGE RankNTypes, RecordWildCards, ScopedTypeVariables         #-}
{-# LANGUAGE TupleSections, TypeFamilies, ViewPatterns                #-}

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
import           Data.Primitive.MutVar
import           Data.Proxy
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
import           GHC.TypeLits
import qualified System.IO                   as IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    [n, p] <- map read.words <$> getLine :: IO [Int]
    ds <- U.unfoldrN n (runParser digit) <$> C.getLine
    withModulus p $ \proxy -> do
        print $ solve proxy n p ds

solve :: forall p . KnownNat p
    => Proxy p -> Int -> Int -> U.Vector Int -> Int
solve _ n prime (U.reverse -> ds)
    | prime == 2 = U.sum $ U.imap (\i x -> if even x then n - i else 0) ds
    | prime == 5 = U.sum $ U.imap (\i x -> if x == 0 || x == 5 then n - i else 0) ds
    | otherwise = U.sum (U.map (\cnt -> cnt*(cnt-1)`quot`2) freq)
  where
    step (acc, _) x = (acc * 10, mkGF x * acc)
    rems :: U.Vector (GF p, GF p)
    rems = U.scanl' step (1,0) ds
    freq :: U.Vector Int
    freq = U.accumulate (+) (U.replicate prime 0)
        . U.map (\r -> (unGF r, 1))
        . U.scanl1 (+)
        $ U.map snd rems

digit :: Parser Int
digit = digitToInt <$> char

withModulus :: (Integral i) => i -> (forall n.KnownNat n => Proxy n -> a) -> a
withModulus n f = case someNatVal (fromIntegral n) of
    Just (SomeNat proxy) -> f proxy
    Nothing              -> error "withModulus failed"

modulusVal :: KnownNat n => proxy n -> Int
modulusVal = fromIntegral . natVal
{-# INLINE modulusVal #-}

modulusVal' :: KnownNat n => Proxy# n -> Int
modulusVal' proxy = fromIntegral $ natVal' proxy
{-# INLINE modulusVal' #-}

newtype GF (p :: Nat) = GF{ unGF :: Int } deriving Eq

mkGF :: forall p . KnownNat p => Int -> GF p
mkGF x = GF (x `mod` modulusVal' (proxy# :: Proxy# p))

instance Show (GF p) where
    show = coerce (show :: Int -> String)

instance (KnownNat p) => Num (GF p) where
    x + y = case coerce x + coerce y of
        xy | xy < m -> coerce xy
           | otherwise -> coerce (xy - m)
      where
        m = modulusVal' (proxy# :: Proxy# p)
    x - y = case coerce x - coerce y of
        xy | xy < 0 -> coerce $ xy + modulusVal' (proxy# :: Proxy# p)
           | otherwise -> coerce xy
    x * y = GF $ coerce x * coerce y `rem` modulusVal' (proxy# :: Proxy# p)
    abs = id
    signum = id
    fromInteger x = GF . fromIntegral $ x `mod` fromIntegral m
      where
        m = modulusVal' (proxy# :: Proxy# p)

instance (KnownNat p) => Fractional (GF p) where
    recip x = coerce $ go (coerce x) m 1 0
      where
        m = modulusVal x
        go !a !b !u !v
            | b > 0 = case a `quot` b of
                q -> go b (a - (q * b)) v (u - (q * v))
            | otherwise = u `mod` m
    fromRational _ = undefined

newtype instance UM.MVector s (GF p) = MV_GF (UM.MVector s Int)
newtype instance U.Vector (GF p)= V_GF (U.Vector Int)

instance U.Unbox (GF p)

instance GM.MVector UM.MVector (GF p) where
    basicLength (MV_GF v) = GM.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (MV_GF v) = MV_GF $ GM.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MV_GF v1) (MV_GF v2) = GM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MV_GF `liftM` GM.basicUnsafeNew n
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MV_GF v) = GM.basicInitialize v
    {-# INLINE basicInitialize #-}
    basicUnsafeReplicate n x = MV_GF `liftM` GM.basicUnsafeReplicate n (coerce x)
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeRead (MV_GF v) i = coerce `liftM` GM.basicUnsafeRead v i
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MV_GF v) i x = GM.basicUnsafeWrite v i (coerce x)
    {-# INLINE basicUnsafeWrite #-}
    basicClear (MV_GF v) = GM.basicClear v
    {-# INLINE basicClear #-}
    basicSet (MV_GF v) x = GM.basicSet v (coerce x)
    {-# INLINE basicSet #-}
    basicUnsafeCopy (MV_GF v1) (MV_GF v2) = GM.basicUnsafeCopy v1 v2
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeMove (MV_GF v1) (MV_GF v2) = GM.basicUnsafeMove v1 v2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeGrow (MV_GF v) n = MV_GF `liftM` GM.basicUnsafeGrow v n
    {-# INLINE basicUnsafeGrow #-}

instance G.Vector U.Vector (GF p) where
    basicUnsafeFreeze (MV_GF v) = V_GF `liftM` G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (V_GF v) = MV_GF `liftM` G.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}
    basicLength (V_GF v) = G.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (V_GF v) = V_GF $ G.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (V_GF v) i = coerce `liftM` G.basicUnsafeIndexM v i
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MV_GF mv) (V_GF v) = G.basicUnsafeCopy mv v
    elemseq _ = seq
    {-# INLINE elemseq #-}


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
