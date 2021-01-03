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
import           Data.Monoid                       hiding (First (..),
                                                    Last (..))
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                          as S
import           Data.Tuple
import qualified Data.Vector                       as V
import qualified Data.Vector.Algorithms.Intro      as Intro
import qualified Data.Vector.Fusion.Bundle.Monadic as MB
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
import           System.IO
import           Unsafe.Coerce

#define MOD 1000000007 /* 998244353 */

main :: IO ()
main = runSolver $ do
    mat <- vectorHW @U.Vector 5 5 int
    return . (<> endlB) . showB $ solve mat

solve :: U.Vector Int -> IntMod
solve (U.map pred -> mat) = runST $ do
        dp <- UM.replicate (shiftL 1 n) 0
        UM.write dp 0 1
        rep1 (shiftL 1 n - 1) $ \s -> do
            let decoded = decompress $ coerce s
            U.forM_ unfixedPos $ \posX -> do
                when (check posX decoded && check' (deleteBS posX decoded)) $ do
                    dps' <- UM.unsafeRead dp
                        (coerce . compress $ deleteBS posX decoded)
                    UM.unsafeModify dp (+dps') (coerce s)
        UM.read dp (shiftL 1 n - 1)
    where
        !n = U.length $ U.elemIndices (-1) mat
        !unfixed = U.filter (flip U.notElem mat) $ U.generate 25 id
        !unfixedPos = U.elemIndices (-1) mat
        !fixed = U.filter (flip U.elem mat) $ U.generate 25 id
        !fixedPos = U.generate 25 (\i -> maybe (-1) id $ U.elemIndex i mat)

        compress :: BitSet -> BitSet
        compress set = U.ifoldl'
            (\acc i pos -> if memberBS pos set then insertBS i acc else acc)
            emptyBS unfixedPos

        decompress :: BitSet -> BitSet
        decompress set = unionBS fixedSet unfixedSet
            where
                sz = sizeBS set
                !x = unfixed U.! (sz - 1)
                fixedSet = U.ifoldl'
                    (\acc pos y -> if 0 <= y && y < x then insertBS pos acc else acc)
                    emptyBS mat
                unfixedSet = U.ifoldl'
                    (\acc i pos -> if memberBS i set then insertBS pos acc else acc)
                    emptyBS unfixedPos

        check :: Int -> BitSet -> Bool
        check xy s
            = memberBS xy s
                && (x == 0 || x == 4 || memberBS (xy - 5) s == memberBS (xy + 5) s)
                && (y == 0 || y == 4 || memberBS (xy - 1) s == memberBS (xy + 1) s)
            where
                (x, y) = quotRem xy 5

        check' :: BitSet -> Bool
        check' set = case fixedPos U.!? x of
            Just (-1) -> True
            Just pos -> check pos set && check' (deleteBS pos set)
            Nothing -> True
            where
                sz = sizeBS set
                x = sz - 1
-------------------------------------------------------------------------------
-- My.Prelude
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
unlinesB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unlinesB f = G.foldr' ((<>) . (<> endlB) . f) mempty
unwordsB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unwordsB f vec | G.null vec = mempty | otherwise = f (G.head vec) <> G.foldr' ((<>) . (B.char7 ' ' <>) . f) mempty (G.tail vec)
gridB :: (G.Vector v a) => Int -> Int -> (a -> B.Builder) -> v a -> B.Builder
gridB h w f mat = F.foldMap ((<> endlB) . unwordsB f) [G.slice (i * w) w mat | i <- [0 .. h - 1]]
sizedB :: (G.Vector v a) => (v a -> B.Builder) -> v a -> B.Builder
sizedB f vec = B.intDec (G.length vec) <> endlB <> f vec
yesnoB :: Bool -> B.Builder
yesnoB = bool (B.string7 "No") (B.string7 "Yes")
pairB :: (a -> B.Builder) -> (b -> B.Builder) -> (a, b) -> B.Builder
pairB f g (x, y) = f x <> B.char7 ' ' <> g y
showB :: (Show a) => a -> B.Builder
showB = B.string7 . show
endlB :: B.Builder
endlB = B.char7 '\n'
{-# INLINE endlB #-}
runSolver :: Parser B.Builder -> IO ()
runSolver solver = do { bs <- C.getContents; case runStateT solver bs of { Just (answer, rest) -> do { unless (C.all isSpace rest) $ do { C.hPutStrLn stderr rest}; B.hPutBuilder stdout answer}; Nothing -> hPutStrLn stderr "parse error"}}
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
integer :: Parser Integer
integer = coerce $ C.readInteger . C.dropWhile isSpace
{-# INLINE integer #-}
integral :: (Integral a) => Parser a
integral = fmap fromIntegral int
{-# INLINE integral #-}
char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}
byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}
bytestring :: Parser C.ByteString
bytestring = do { skipSpaces; gets (C.findIndex isSpace) >>= \case { Just i -> state (C.splitAt i); Nothing -> state (flip (,) C.empty)}}
{-# INLINE bytestring #-}
skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}
vector :: (G.Vector v a) => Parser a -> Parser (v a)
vector p = G.unfoldr (runParser p) <$> takeLine
{-# INLINE vector #-}
vectorN :: (G.Vector v a) => Int -> Parser a -> Parser (v a)
vectorN n p = G.unfoldrN n (runParser p) <$> takeLine
{-# INLINE vectorN #-}
vectorH :: (G.Vector v a) => Int -> Parser a -> Parser (v a)
vectorH h p = G.unfoldrN h (runParser p) <$> takeLines h
{-# INLINE vectorH #-}
vectorW :: (G.Vector v a) => Int -> Parser a -> Parser (v a)
vectorW = vectorN
{-# INLINE vectorW #-}
vectorHW :: (G.Vector v a) => Int -> Int -> Parser a -> Parser (v a)
vectorHW h w p = G.unfoldrN (h * w) (runParser p) <$> takeLines h
{-# INLINE vectorHW #-}
gridHW :: Int -> Int -> Parser (U.Vector Char)
gridHW h w = U.unfoldrN (h * w) (runParser char) . C.filter (/= '\n') <$> takeLines h
{-# INLINE gridHW #-}
takeLine :: Parser C.ByteString
takeLine = do { skipSpaces; state $ C.span (/= '\n')}
{-# INLINE takeLine #-}
takeLines :: Int -> Parser C.ByteString
takeLines n = do { skipSpaces; gets (drop (n - 1) . C.elemIndices '\n') >>= \case { (i : _) -> state (C.splitAt (i + 1)); [] -> state (flip (,) C.empty)}}
{-# INLINE takeLines #-}
neighbor4 :: (Applicative f) => Int -> Int -> Int -> (Int -> f ()) -> f ()
neighbor4 h w xy f = when (x /= 0) (f $ xy - w) *> when (y /= 0) (f $ xy - 1) *> when (y /= w - 1) (f $ xy + 1) *> when (x /= h - 1) (f $ xy + w) where { (!x, !y) = quotRem xy w}
{-# INLINE neighbor4 #-}
binarySearchM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
binarySearchM low high p = go low high where { go !low !high | high <= low = return high | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid) where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE binarySearchM #-}
binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch low high p = runIdentity $ binarySearchM low high (return . p)
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
-- Data.BitSet
-------------------------------------------------------------------------------
newtype BitSet = BitSet{getBitSet :: Int} deriving (Eq, Ord)
instance Show BitSet where { showsPrec p xs = showParen (p > 10) $ showString "fromList " . shows (toList xs)}
instance IsList BitSet where { type Item BitSet = Int; fromList = BitSet . F.foldl' (\ acc x -> acc .|. unsafeShiftL 1 x) 0; toList bs = filter (`memberBS` bs) [0 .. 63]}
emptyBS :: BitSet
emptyBS = BitSet 0
singletonBS :: Int -> BitSet
singletonBS (I# i#) = BitSet (I# (uncheckedIShiftL# 1# i#))
insertBS :: Int -> BitSet -> BitSet
insertBS (I# i#) (BitSet (I# bs#)) = BitSet (I# ((uncheckedIShiftL# 1# i#) `orI#` bs#))
deleteBS :: Int -> BitSet -> BitSet
deleteBS (I# i#) (BitSet (I# bs#)) = BitSet (I# (notI# (uncheckedIShiftL# 1# i#) `andI#` bs#))
memberBS :: Int -> BitSet -> Bool
memberBS (I# i#) (BitSet (I# bs#)) = isTrue# (uncheckedIShiftRL# bs# i# `andI#` 1#)
notMemberBS :: Int -> BitSet -> Bool
notMemberBS i = not . memberBS i
nullBS :: BitSet -> Bool
nullBS = (== 0) . coerce @BitSet @Int
sizeBS :: BitSet -> Int
sizeBS = coerce (popCount @Int)
isSubsetOf :: BitSet -> BitSet -> Bool
isSubsetOf x y = intersectionBS x y == x
unionBS :: BitSet -> BitSet -> BitSet
unionBS = coerce ((.|.) @Int)
complementBS :: BitSet -> BitSet
complementBS = coerce (complement @Int)
differenceBS :: BitSet -> BitSet -> BitSet
differenceBS x y = intersectionBS x (complementBS y)
intersectionBS :: BitSet -> BitSet -> BitSet
intersectionBS = coerce ((.&.) @Int)
findMinBS :: BitSet -> Int
findMinBS = coerce (countTrailingZeros @Int)
findMaxBS :: BitSet -> Int
findMaxBS = (63 -) . coerce (countLeadingZeros @Int)
deleteMinBS :: BitSet -> BitSet
deleteMinBS (BitSet x) = BitSet (x .&. (x - 1))
deleteMaxBS :: BitSet -> BitSet
deleteMaxBS x = deleteBS (findMaxBS x) x
deleteFindMin :: BitSet -> (Int, BitSet)
deleteFindMin x = (findMinBS x, deleteMinBS x)
deleteFindMax :: BitSet -> (Int, BitSet)
deleteFindMax x = let { i = findMaxBS x} in (i, deleteBS i x)
minView :: BitSet -> Maybe (Int, BitSet)
minView x | x /= BitSet 0 = Just $ deleteFindMin x | otherwise = Nothing
maxView :: BitSet -> Maybe (Int, BitSet)
maxView x | x /= BitSet 0 = Just $ deleteFindMax x | otherwise = Nothing
newtype instance  UM.MVector s BitSet = MV_BitSet (UM.MVector s Int)
newtype instance  U.Vector BitSet = V_BitSet (U.Vector Int)
instance U.Unbox BitSet
instance GM.MVector UM.MVector BitSet where { basicLength (MV_BitSet v) = GM.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (MV_BitSet v) = MV_BitSet $ GM.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicOverlaps (MV_BitSet v1) (MV_BitSet v2) = GM.basicOverlaps v1 v2; {-# INLINE basicOverlaps #-}; basicUnsafeNew n = MV_BitSet `liftM` GM.basicUnsafeNew n; {-# INLINE basicUnsafeNew #-}; basicInitialize (MV_BitSet v) = GM.basicInitialize v; {-# INLINE basicInitialize #-}; basicUnsafeReplicate n x = MV_BitSet `liftM` GM.basicUnsafeReplicate n (coerce x); {-# INLINE basicUnsafeReplicate #-}; basicUnsafeRead (MV_BitSet v) i = coerce `liftM` GM.basicUnsafeRead v i; {-# INLINE basicUnsafeRead #-}; basicUnsafeWrite (MV_BitSet v) i x = GM.basicUnsafeWrite v i (coerce x); {-# INLINE basicUnsafeWrite #-}; basicClear (MV_BitSet v) = GM.basicClear v; {-# INLINE basicClear #-}; basicSet (MV_BitSet v) x = GM.basicSet v (coerce x); {-# INLINE basicSet #-}; basicUnsafeCopy (MV_BitSet v1) (MV_BitSet v2) = GM.basicUnsafeCopy v1 v2; {-# INLINE basicUnsafeCopy #-}; basicUnsafeMove (MV_BitSet v1) (MV_BitSet v2) = GM.basicUnsafeMove v1 v2; {-# INLINE basicUnsafeMove #-}; basicUnsafeGrow (MV_BitSet v) n = MV_BitSet `liftM` GM.basicUnsafeGrow v n; {-# INLINE basicUnsafeGrow #-}}
instance G.Vector U.Vector BitSet where { basicUnsafeFreeze (MV_BitSet v) = V_BitSet `liftM` G.basicUnsafeFreeze v; {-# INLINE basicUnsafeFreeze #-}; basicUnsafeThaw (V_BitSet v) = MV_BitSet `liftM` G.basicUnsafeThaw v; {-# INLINE basicUnsafeThaw #-}; basicLength (V_BitSet v) = G.basicLength v; {-# INLINE basicLength #-}; basicUnsafeSlice i n (V_BitSet v) = V_BitSet $ G.basicUnsafeSlice i n v; {-# INLINE basicUnsafeSlice #-}; basicUnsafeIndexM (V_BitSet v) i = coerce `liftM` G.basicUnsafeIndexM v i; {-# INLINE basicUnsafeIndexM #-}; basicUnsafeCopy (MV_BitSet mv) (V_BitSet v) = G.basicUnsafeCopy mv v; elemseq _ = seq; {-# INLINE elemseq #-}}
