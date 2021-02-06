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
import           Foreign                           hiding (sizeOf, void)
import           GHC.Exts
import           GHC.TypeLits
import           System.IO
import           Unsafe.Coerce

#define MOD 1000000007 /* 998244353 */

main :: IO ()
main = runSolver $ do
    n <- int
    xys <- vectorH @U.Vector n ((,) <$> int <*> int)
    m <- int
    ops <- vectorH @U.Vector m $ do
        int >>= \case
            1 -> pure (1, 0)
            2 -> pure (2, 0)
            3 -> (,) 3 <$> int
            4 -> (,) 4 <$> int
    q <- int
    qs <- vectorH @U.Vector q ((,) <$> int <*> int1)
    return
        . unlinesB (pairB B.intDec B.intDec)
        $ solve n xys m ops q qs

-- 1: (x, y) -> (y,-x)
-- 2: (x, y) -> (-y,x)
-- 3: (x, y) -> (2 * p - x, y)
-- 4: (x, y) -> (x, 2 * p - y)

solve :: Int -> U.Vector (Int, Int)
    -> Int -> U.Vector (Int, Int)
    -> Int -> U.Vector (Int, Int)
    -> U.Vector (Int, Int)
solve n xys m ops q qs = U.map query qs
    where
        query (a, (xys U.!) -> (x, y)) = case appMat3x3 (table U.! a) x y 1 of
            (x', y', _) -> (x', y')
        mat0 :: Mat3x3 Int
        mat0 = fromList [1,0,0,0,1,0,0,0,1]
        !table = U.scanl' (\acc op -> opMat op * acc) mat0 ops

opMat :: (Int, Int) -> Mat3x3 Int
opMat (1, _) = fromList [0,1,0,-1,0,0,0,0,1]
opMat (2, _) = fromList [0,-1,0,1,0,0,0,0,1]
opMat (3, p) = fromList [-1,0,2*p,0,1,0,0,0,1]
opMat (4, p) = fromList [1,0,0,0,-1,2*p,0,0,1]

data Mat3x3 a = Mat3x3 !Int !ByteArray

instance (Prim a, Eq a) => Eq (Mat3x3 a) where
    (==) = (==) `on` toList

instance (Prim a, Show a) => Show (Mat3x3 a) where
    show = show . toList

instance (Prim a) => IsList (Mat3x3 a) where
    type Item (Mat3x3 a) = a
    fromList = Mat3x3 0 . byteArrayFromListN 9
    toList (Mat3x3 o ba) = map (indexByteArray ba) [o..o+8]

appMat3x3 :: (Prim a, Num a) => Mat3x3 a -> a -> a -> a -> (a, a, a)
appMat3x3 (Mat3x3 o ba) x y z = (x', y', z')
    where
        !x' = x * indexByteArray ba (o + 0)
            + y * indexByteArray ba (o + 1)
            + z * indexByteArray ba (o + 2)
        !y' = x * indexByteArray ba (o + 3)
            + y * indexByteArray ba (o + 4)
            + z * indexByteArray ba (o + 5)
        !z' = x * indexByteArray ba (o + 6)
            + y * indexByteArray ba (o + 7)
            + z * indexByteArray ba (o + 8)

rep3 :: (Int -> ST s ()) -> ST s ()
rep3 f = f 0 *> f 1 *> f 2
{-# INLINE rep3 #-}

rep9 :: (Int -> ST s ()) -> ST s ()
rep9 f = f 0 *> f 1 *> f 2 *> f 3 *> f 4 *> f 5 *> f 6 *> f 7 *> f 8

createMat3x3 :: forall a.(Prim a) => (forall s.MutableByteArray s -> ST s ()) -> Mat3x3 a
createMat3x3 run = runST $ do
    mba <- newByteArray (sizeOf @a undefined * 9)
    run mba
    Mat3x3 0 <$> unsafeFreezeByteArray mba

genMat3x3 :: (Prim a) => (Int -> Int -> a) -> Mat3x3 a
genMat3x3 f = createMat3x3 $ \mba -> do
    rep3 $ \i -> do
        rep3 $ \j -> do
            writeByteArray mba (3 * i + j) (f i j)

instance (Prim a, Num a) => Num (Mat3x3 a) where
    (Mat3x3 ox xs) + (Mat3x3 oy ys) = createMat3x3 $ \mba -> rep9 $ \i -> do
        writeByteArray @a mba i
            $ indexByteArray xs (ox + i) + indexByteArray ys (oy + i)
    (Mat3x3 ox xs) - (Mat3x3 oy ys) = createMat3x3 $ \mba -> rep9 $ \i -> do
        writeByteArray @a mba i
            $ indexByteArray xs (ox + i) - indexByteArray ys (oy + i)
    (Mat3x3 ox xs) * (Mat3x3 oy ys) = createMat3x3 $ \mba -> do
        rep3 $ \i -> do
            let !ox' = ox + 3 * i
            rep3 $ \j -> do
                let !oy' = oy + j
                writeByteArray @a mba (3 * i + j)
                    $ indexByteArray xs ox'       * indexByteArray ys oy'
                    + indexByteArray xs (ox' + 1) * indexByteArray ys (oy' + 3)
                    + indexByteArray xs (ox' + 2) * indexByteArray ys (oy' + 6)
    negate (Mat3x3 ox xs) = createMat3x3 $ \mba -> rep9 $ \i -> do
        writeByteArray @a mba i . negate
            $ indexByteArray xs (ox + i)
    abs = id
    signum = id
    fromInteger x = createMat3x3 $ \mba -> do
        setByteArray @a mba 0 9 0
        writeByteArray @a mba 0 (fromIntegral x)
        writeByteArray @a mba 4 (fromIntegral x)
        writeByteArray @a mba 8 (fromIntegral x)

data instance UM.MVector s (Mat3x3 a) = MV_Mat3x3 !Int !Int !(MutableByteArray s)
data instance U.Vector (Mat3x3 a) = V_Mat3x3 !Int !Int !ByteArray

instance (Prim a) => U.Unbox (Mat3x3 a)

instance (Prim a) => GM.MVector UM.MVector (Mat3x3 a) where
    basicLength (MV_Mat3x3 _ n _) = quot n 9
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (MV_Mat3x3 o _ mba) = MV_Mat3x3 (o + 9 * i) (9 * n) mba
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MV_Mat3x3 ox nx xs) (MV_Mat3x3 oy ny ys)
        = sameMutableByteArray xs ys
        && ox < oy + ny && oy < ox + nx
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MV_Mat3x3 0 (9 * n) <$> newByteArray (sizeOf @a undefined * 9 * n)
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MV_Mat3x3 o n mba) = fillByteArray mba (sz * o) (sz * n) 0
        where
            sz = sizeOf @a undefined
    {-# INLINE basicInitialize #-}
    basicUnsafeRead (MV_Mat3x3 o n mba) i = do
        dst <- newByteArray (sz * 9)
        copyMutableByteArray dst 0 mba (sz * o) (sz * 9)
        Mat3x3 0 <$> unsafeFreezeByteArray  dst
        where
            sz = sizeOf @a undefined
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MV_Mat3x3 o _ mba) i (Mat3x3 o' ba) =
        copyByteArray mba (sz * o) ba (sz * o') (sz * 9)
        where
            sz = sizeOf @a undefined
    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeCopy (MV_Mat3x3 o n dst) (MV_Mat3x3 o' n' src) =
        copyMutableByteArray dst (sz * o) src (sz * o') (sz * n')
        where
            sz = sizeOf @a undefined
    {-# INLINE basicUnsafeCopy #-}

instance (Prim a) => G.Vector U.Vector (Mat3x3 a) where
    basicUnsafeFreeze (MV_Mat3x3 o n mba) = V_Mat3x3 o n <$> unsafeFreezeByteArray mba
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (V_Mat3x3 o n ba) = MV_Mat3x3 o n <$> unsafeThawByteArray ba
    {-# INLINE basicUnsafeThaw #-}
    basicLength (V_Mat3x3 _ n _) = quot n 9
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (V_Mat3x3 o _ ba) = V_Mat3x3 (o + 9 * i) (9 * n) ba
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (V_Mat3x3 o n ba) i = return $! Mat3x3 (o + 9 * i) ba
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MV_Mat3x3 o n dst) (V_Mat3x3 o' n' src) =
        copyByteArray dst (sz * o) src (sz * o') (sz * n')
        where
            sz = sizeOf @a undefined
    elemseq _ = seq
    {-# INLINE elemseq #-}

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
showLnB :: (Show a) => a -> B.Builder
showLnB = B.string7 . flip shows "\n"
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
