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
import           Data.Monoid hiding (Last(..), First(..))
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                          as S
import           Data.Tuple
import qualified Data.Vector                       as V
import qualified Data.Vector.Algorithms.Intro      as Intro
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Fusion.Bundle.Monadic as MB
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
import           System.IO
import           Unsafe.Coerce

#define MOD 998244353



main :: IO ()
main = do
    [n, q] <- map read.words <$> getLine
    qs <- U.unfoldrN n (runParser $ (,,) <$> int1 <*> int <*> int) <$> C.getContents
    putStr.unlines.map show.U.toList $ solve n q qs

type F = Int
instance Semigroup Int where
    10 <> x = x
    x <> _ = x
    {-# INLINE (<>) #-}

instance Monoid Int where
    mempty = 10

type M = (Sum IntMod, Sum IntMod)

instance MonoidAction F M where
    appMonoid 10 x = x
    appMonoid d (Sum x, Sum y) = (Sum $ IntMod d * y, Sum y)

solve :: Int -> Int -> U.Vector (Int, Int, Int) -> U.Vector IntMod
solve n q qs = runST $ do
    seg <- buildSegTree @F @M
        $ U.map (\d -> (Sum d, Sum d)) ds
    U.forM qs $ \(l,r,d) -> do
        appFromTo seg l r d
        getSum . fst <$> mappendAll seg
  where
    ds :: U.Vector IntMod
    ds = U.reverse .U.scanl' (*) 1
        $ U.replicate (n - 1) 10

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------
rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (stream 0 n)
{-# INLINE rep #-}
rep1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep1 n = flip MS.mapM_ (stream 1 (n + 1))
{-# INLINE rep1 #-}
rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev n = flip MS.mapM_ (streamR 0 n)
{-# INLINE rev #-}
rev1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev1 n = flip MS.mapM_ (streamR 1 (n + 1))
{-# INLINE rev1 #-}
stream :: (Monad m) => Int -> Int -> MS.Stream m Int
stream !l !r = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream #-}
streamR :: (Monad m) => Int -> Int -> MS.Stream m Int
streamR !l !r = MS.Stream step (r - 1) where { step x | x >= l = return $ MS.Yield x (x - 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] streamR #-}
stream' :: (Monad m) => Int -> Int -> Int -> MS.Stream m Int
stream' !l !r !d = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + d) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream' #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL :: Int -> Int -> Int
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL :: Int -> Int -> Int
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
skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}
binarySearchM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
binarySearchM low high p = go low high where { go !low !high | high <= low = return high | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid) where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE binarySearchM #-}
binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch low high p = runIdentity (binarySearchM low high (return . p))
{-# INLINE binarySearch #-}
radixSort :: U.Vector Int -> U.Vector Int
radixSort v = F.foldl' step v [0, 16, 32, 48] where { mask k x = unsafeShiftRL x k .&. 65535; step v k = U.create $ do { pos <- UM.unsafeNew 65537; UM.set pos 0; U.forM_ v $ \ x -> do { UM.unsafeModify pos (+ 1) (mask k x + 1)}; rep 65535 $ \ i -> do { fi <- UM.unsafeRead pos i; UM.unsafeModify pos (+ fi) (i + 1)}; res <- UM.unsafeNew $ U.length v; U.forM_ v $ \ x -> do { let { !masked = mask k x}; i <- UM.unsafeRead pos masked; UM.unsafeWrite pos masked $ i + 1; UM.unsafeWrite res i x}; return res}}
{-# INLINE radixSort #-}
encode32x2 :: Int -> Int -> Int
encode32x2 x y = unsafeShiftL x 32 .|. y
{-# INLINE encode32x2 #-}
decode32x2 :: Int -> (Int, Int)
decode32x2 xy = let { !x = unsafeShiftRL xy 32; !y = xy .&. 4294967295} in (x, y)
{-# INLINE decode32x2 #-}
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
(I# x#) *% (I# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of { z# -> case timesWord2# z# im# of { (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of { v# | isTrue# (geWord# v# m#) -> I# (word2Int# (plusWord# v# m#)) | otherwise -> I# (word2Int# v#)}}} where { m# = int2Word# MOD#; im# = plusWord# (quotWord# 18446744073709551615## m#) 1##}
{-# INLINE (*%) #-}
(/%) :: Int -> Int -> Int
(I# x#) /% (I# y#) = go# y# MOD# 1# 0# where { go# a# b# u# v# | isTrue# (b# ># 0#) = case a# `quotInt#` b# of { q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))} | otherwise = I# ((x# *# (u# +# MOD#)) `remInt#` MOD#)}
{-# INLINE (/%) #-}
(^%) :: Int -> Int -> Int
x ^% n | n > 0 = go 1 x n | n == 0 = 1 | otherwise = go 1 (1 /% x) (-n) where { go !acc !y !m | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1) | m == 1 = acc *% y | otherwise = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)}
newtype IntMod = IntMod{getIntMod :: Int} deriving newtype (Eq, Ord, Read, Show, Real, Prim)
intMod :: (Integral a) => a -> IntMod
intMod x = fromIntegral $ mod (fromIntegral x) MOD
{-# INLINE intMod #-}
intModValidate :: IntMod -> Bool
intModValidate (IntMod x) = 0 <= x && x < MOD
{-# INLINE intModValidate #-}
instance Bounded IntMod where { minBound = IntMod 0; maxBound = IntMod $ modulus - 1}
instance Enum IntMod where { toEnum = intMod; fromEnum = coerce}
instance Integral IntMod where { quotRem x y = (x / y, x - x / y * y); toInteger = coerce (toInteger @Int)}
instance Num IntMod where { (+) = coerce (+%); (-) = coerce (-%); (*) = coerce (*%); abs = id; signum = const (IntMod 1); fromInteger x = coerce @Int @IntMod . fromInteger $ mod x modulus}
instance Fractional IntMod where { (/) = coerce (/%); fromRational q = fromInteger (numerator q) / fromInteger (denominator q)}
newtype instance  UM.MVector s IntMod = MV_IntMod (UM.MVector s Int)
newtype instance  U.Vector IntMod = V_IntMod (U.Vector Int)
instance U.Unbox IntMod
instance GM.MVector UM.MVector IntMod where { basicLength (MV_IntMod v) = GM.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (MV_IntMod v) = MV_IntMod $ GM.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicOverlaps (MV_IntMod v1) (MV_IntMod v2) = GM.basicOverlaps v1 v2; {-# INLINE basicOverlaps #-}; basicUnsafeNew n = MV_IntMod `liftM` GM.basicUnsafeNew n; {-# INLINE basicUnsafeNew #-}; basicInitialize (MV_IntMod v) = GM.basicInitialize v; {-# INLINE basicInitialize #-}; basicUnsafeReplicate n x = MV_IntMod `liftM` GM.basicUnsafeReplicate n (coerce x); {-# INLINE basicUnsafeReplicate #-}; basicUnsafeRead (MV_IntMod v) i = coerce `liftM` GM.basicUnsafeRead v i; {-# INLINE basicUnsafeRead #-}; basicUnsafeWrite (MV_IntMod v) i x = GM.basicUnsafeWrite v i (coerce x); {-# INLINE basicUnsafeWrite #-}; basicClear (MV_IntMod v) = GM.basicClear v; {-# INLINE basicClear #-}; basicSet (MV_IntMod v) x = GM.basicSet v (coerce x); {-# INLINE basicSet #-}; basicUnsafeCopy (MV_IntMod v1) (MV_IntMod v2) = GM.basicUnsafeCopy v1 v2; {-# INLINE basicUnsafeCopy #-}; basicUnsafeMove (MV_IntMod v1) (MV_IntMod v2) = GM.basicUnsafeMove v1 v2; {-# INLINE basicUnsafeMove #-}; basicUnsafeGrow (MV_IntMod v) n = MV_IntMod `liftM` GM.basicUnsafeGrow v n; {-# INLINE basicUnsafeGrow #-}}
instance G.Vector U.Vector IntMod where { basicUnsafeFreeze (MV_IntMod v) = V_IntMod `liftM` G.basicUnsafeFreeze v; {-# INLINE basicUnsafeFreeze #-}; basicUnsafeThaw (V_IntMod v) = MV_IntMod `liftM` G.basicUnsafeThaw v; {-# INLINE basicUnsafeThaw #-}; basicLength (V_IntMod v) = G.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (V_IntMod v) = V_IntMod $ G.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicUnsafeIndexM (V_IntMod v) i = coerce `liftM` G.basicUnsafeIndexM v i; {-# INLINE basicUnsafeIndexM #-}; basicUnsafeCopy (MV_IntMod mv) (V_IntMod v) = G.basicUnsafeCopy mv v; elemseq _ = seq; {-# INLINE elemseq #-}}
-------------------------------------------------------------------------------
-- Data.SegTree
-------------------------------------------------------------------------------
class (Monoid f) => MonoidAction f a where { appMonoid :: f -> a -> a}
instance MonoidAction () m where { appMonoid = flip const; {-# INLINE appMonoid #-}}
instance MonoidAction (Sum Int) (Min Int) where { appMonoid (Sum x) (Min y) | y /= maxBound = Min (x + y) | otherwise = Min y; {-# INLINE appMonoid #-}}
instance MonoidAction (Sum Int) (Max Int) where { appMonoid (Sum x) (Max y) | y /= minBound = Max (x + y) | otherwise = Max y; {-# INLINE appMonoid #-}}
instance MonoidAction (Sum Int) (Sum Int, Sum Int) where { appMonoid (Sum x) (Sum y, Sum size) = (Sum (y + x * size), Sum size)}
instance MonoidAction (Dual (Maybe (Last (Min Int)))) (Min Int) where { appMonoid (Dual Nothing) y = y; appMonoid (Dual (Just x)) _ = coerce x}
instance MonoidAction (Dual (Maybe (Last (Max Int)))) (Max Int) where { appMonoid (Dual Nothing) y = y; appMonoid (Dual (Just x)) _ = coerce x}
instance MonoidAction (Min Int) (Min Int) where { appMonoid = (<>); {-# INLINE appMonoid #-}}
instance MonoidAction (Max Int) (Max Int) where { appMonoid = (<>); {-# INLINE appMonoid #-}}
data SegTree s f a = SegTree{getSegTree :: !(UM.MVector s a), getDualSegTree :: !(UM.MVector s f), sizeSegTree :: !Int, heightSegTree :: !Int}
newSegTree :: (Monoid f, U.Unbox f, Monoid a, U.Unbox a, PrimMonad m) => Int -> m (SegTree (PrimState m) f a)
newSegTree n0 = do { seg <- UM.replicate (2 * n) mempty; dseg <- UM.replicate n mempty; return $ SegTree seg dseg n (63 - countLeadingZeros n)} where { !n = extendToPowerOfTwo n0}
buildSegTree :: (Monoid f, U.Unbox f, Monoid a, U.Unbox a, PrimMonad m) => U.Vector a -> m (SegTree (PrimState m) f a)
buildSegTree xs = do { seg <- UM.replicate (2 * n) mempty; dseg <- UM.replicate n mempty; U.unsafeCopy (UM.unsafeSlice n (U.length xs) seg) xs; let { st = SegTree seg dseg n (63 - countLeadingZeros n)}; rev1 (n - 1) $ \ i -> do { pullSegTree st i}; return st} where { !n = extendToPowerOfTwo $ U.length xs}
readSegTree :: (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> Int -> m a
readSegTree st k0 = do { let { !k = k0 + sizeSegTree st}; rev1 (heightSegTree st) $ \ i -> do { pushSegTree st (unsafeShiftR k i)}; UM.unsafeRead (getSegTree st) k}
{-# INLINE readSegTree #-}
writeSegTree :: (MonoidAction f a, Semigroup a, U.Unbox a, U.Unbox f, PrimMonad m) => SegTree (PrimState m) f a -> Int -> a -> m ()
writeSegTree st k0 v = do { let { !k = k0 + sizeSegTree st}; rev1 (heightSegTree st) $ \ i -> do { pushSegTree st (unsafeShiftR k i)}; UM.unsafeWrite (getSegTree st) k v; rep1 (heightSegTree st) $ \ i -> do { pullSegTree st (unsafeShiftR k i)}}
{-# INLINE writeSegTree #-}
modifySegTree :: (MonoidAction f a, Semigroup a, U.Unbox f, U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> (a -> a) -> Int -> m ()
modifySegTree st f k0 = do { let { !k = k0 + sizeSegTree st}; rev1 (heightSegTree st) $ \ i -> do { pushSegTree st (unsafeShiftR k i)}; UM.unsafeModify (getSegTree st) f k; rep1 (heightSegTree st) $ \ i -> do { pullSegTree st (unsafeShiftR k i)}}
{-# INLINE modifySegTree #-}
mappendFromTo :: (MonoidAction f a, Monoid a, U.Unbox f, U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> Int -> Int -> m a
mappendFromTo st l0 r0 = do { let { !l = l0 + sizeSegTree st; !r = r0 + sizeSegTree st}; rev1 (heightSegTree st) $ \ i -> do { when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do { pushSegTree st (unsafeShiftR l i)}; when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do { pushSegTree st (unsafeShiftR r i)}}; fix (\ loop !accL !accR !l' !r' -> do { if l' < r' then do { !accL' <- if l .&. 1 == 1 then (accL <>) <$!> UM.unsafeRead (getSegTree st) l else return accL; !accR' <- if r .&. 1 == 1 then (<> accR) <$!> UM.unsafeRead (getSegTree st) (r - 1) else return accR; loop accL' accR' (unsafeShiftR (l' + l' .&. 1) 1) (unsafeShiftR (r' - r' .&. 1) 1)} else return $! accL <> accR}) mempty mempty l r}
{-# INLINE mappendFromTo #-}
mappendAll :: (U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> m a
mappendAll st = UM.unsafeRead (getSegTree st) 1
{-# INLINE mappendAll #-}
appAt :: (MonoidAction f a, Semigroup a, U.Unbox f, U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> Int -> f -> m ()
appAt st k f = modifySegTree st (appMonoid f) k
{-# INLINE appAt #-}
appFromTo :: (MonoidAction f a, Semigroup a, U.Unbox f, U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> Int -> Int -> f -> m ()
appFromTo st l0 r0 f = when (l0 < r0) $ do { let { !l = l0 + sizeSegTree st; !r = r0 + sizeSegTree st}; rev1 (heightSegTree st) $ \ i -> do { when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do { pushSegTree st (unsafeShiftRL l i)}; when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do { pushSegTree st (unsafeShiftRL (r - 1) i)}}; fix (\ loop !l' !r' -> when (l' < r') $ do { when (l' .&. 1 == 1) $ do { evalAt st l' f}; when (r' .&. 1 == 1) $ do { evalAt st (r' - 1) f}; loop (unsafeShiftR (l' + l' .&. 1) 1) (unsafeShiftR (r' - r' .&. 1) 1)}) l r; rep1 (heightSegTree st) $ \ i -> do { when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do { pullSegTree st (unsafeShiftRL l i)}; when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do { pullSegTree st (unsafeShiftRL (r - 1) i)}}}
{-# INLINE appFromTo #-}
evalAt :: (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> Int -> f -> m ()
evalAt st k f = do { tk <- UM.unsafeModify (getSegTree st) (appMonoid f) k; when (k < sizeSegTree st) $ do { UM.unsafeModify (getDualSegTree st) (f <>) k}}
{-# INLINE evalAt #-}
pushSegTree :: (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> Int -> m ()
pushSegTree st k = do { fk <- UM.unsafeRead (getDualSegTree st) k; UM.unsafeWrite (getDualSegTree st) k mempty; evalAt st (2 * k) fk; evalAt st (2 * k + 1) fk}
{-# INLINE pushSegTree #-}
pullSegTree :: (Semigroup a, U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> Int -> m ()
pullSegTree st k = do { (<>) <$> UM.unsafeRead (getSegTree st) (2 * k) <*> UM.unsafeRead (getSegTree st) (2 * k + 1) >>= UM.unsafeWrite (getSegTree st) k}
{-# INLINE pullSegTree #-}
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1 | otherwise = 1
