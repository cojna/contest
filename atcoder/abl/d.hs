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

#define MOD 1000000007

main :: IO ()
main = do
    [n, k] <- map read.words <$> getLine
    xs <- U.unfoldrN n (runParser int) <$> C.getContents
    print $ solve n k xs

solve :: Int -> Int -> U.Vector Int -> Int
solve n k xs = runST $ do
    seg <- buildSegTree @(Max Int)
        $ U.replicate 300300 (Max 0)
    U.forM_ xs $ \x -> do
        x' <- getMax <$> mappendFromTo seg (max 0 $ x - k) (min 300300 $ x + k + 1)
        old <- readSegTree seg x
        writeSegTree seg x (old <> Max (x' + 1))
    getMax <$> mappendAll seg

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
-- Data.SegTree.Generic
-------------------------------------------------------------------------------
newtype SegTree mv s a = SegTree{getSegTree :: mv s a}
newSegTree :: (Monoid a, GM.MVector mv a, PrimMonad m) => Int -> m (SegTree mv (PrimState m) a)
newSegTree n = SegTree <$> GM.replicate (2 * extendToPowerOfTwo n) mempty
buildSegTree :: (Monoid a, PrimMonad m, G.Vector v a) => v a -> m (SegTree (G.Mutable v) (PrimState m) a)
buildSegTree vec = do { let { n = extendToPowerOfTwo $ G.length vec}; tree <- GM.replicate (2 * n) mempty; G.unsafeCopy (GM.unsafeSlice n (G.length vec) tree) vec; flip fix (n - 1) $ \ loop !i -> when (i >= 1) $ do { mappend <$> GM.unsafeRead tree (unsafeShiftL i 1) <*> GM.unsafeRead tree (unsafeShiftL i 1 .|. 1) >>= GM.unsafeWrite tree i; loop (i - 1)}; return $ SegTree tree}
readSegTree :: (Monoid a, PrimMonad m, GM.MVector mv a) => SegTree mv (PrimState m) a -> Int -> m a
readSegTree segtree k = do { let { tree = getSegTree segtree}; let { n = unsafeShiftR (GM.length tree) 1}; GM.unsafeRead tree (k + n)}
{-# INLINE readSegTree #-}
pullSegTree :: (Monoid a, PrimMonad m, GM.MVector mv a) => SegTree mv (PrimState m) a -> Int -> m ()
pullSegTree seg k = do { let { tree = getSegTree seg}; mappend <$> GM.unsafeRead tree (unsafeShiftL k 1) <*> GM.unsafeRead tree (unsafeShiftL k 1 .|. 1) >>= GM.unsafeWrite tree k}
{-# INLINE pullSegTree #-}
writeSegTree :: (Monoid a, PrimMonad m, GM.MVector mv a) => SegTree mv (PrimState m) a -> Int -> a -> m ()
writeSegTree segtree k v = do { let { tree = getSegTree segtree}; let { n = unsafeShiftR (GM.length tree) 1}; GM.unsafeWrite tree (k + n) v; flip fix (unsafeShiftR (k + n) 1) $ \ loop !i -> when (i > 0) $ do { pullSegTree segtree i; loop $ unsafeShiftR i 1}}
{-# INLINE writeSegTree #-}
modifySegTree :: (Monoid a, PrimMonad m, GM.MVector mv a) => SegTree mv (PrimState m) a -> (a -> a) -> Int -> m ()
modifySegTree segtree f k = do { let { tree = getSegTree segtree}; let { n = unsafeShiftR (GM.length tree) 1}; GM.unsafeModify tree f (k + n); flip fix (unsafeShiftR (k + n) 1) $ \ loop !i -> when (i > 0) $ do { pullSegTree segtree i; loop $ unsafeShiftR i 1}}
{-# INLINE modifySegTree #-}
mappendFromTo :: (Monoid a, PrimMonad m, GM.MVector mv a) => SegTree mv (PrimState m) a -> Int -> Int -> m a
mappendFromTo segtree l r = do { let { tree = getSegTree segtree}; let { n = unsafeShiftR (GM.length tree) 1}; fix (\ loop !accL !accR !l !r -> do { if l < r then do { accL' <- if l .&. 1 == 1 then mappend accL <$> GM.unsafeRead tree l else return accL; accR' <- if r .&. 1 == 1 then flip mappend accR <$> GM.unsafeRead tree (r - 1) else return accR; loop accL' accR' (unsafeShiftR (l + l .&. 1) 1) (unsafeShiftR (r - r .&. 1) 1)} else return $! accL <> accR}) mempty mempty (l + n) (r + n)}
{-# INLINE mappendFromTo #-}
mappendTo :: (Monoid a, PrimMonad m, GM.MVector mv a) => SegTree mv (PrimState m) a -> Int -> m a
mappendTo segtree = mappendFromTo segtree 0
{-# INLINE mappendTo #-}
mappendAll :: (PrimMonad m, GM.MVector mv a) => SegTree mv (PrimState m) a -> m a
mappendAll segtree = GM.unsafeRead (getSegTree segtree) 1
{-# INLINE mappendAll #-}
maxRightSegTree :: (Monoid a, PrimMonad m, GM.MVector mv a) => SegTree mv (PrimState m) a -> Int -> (a -> Bool) -> m Int
maxRightSegTree segtree l p = do { let { tree = getSegTree segtree}; let { !n = unsafeShiftR (GM.length tree) 1}; fix (\ oloop !oacc !ol -> do { let { ol' = unsafeShiftR ol (countTrailingZeros ol)}; oacc' <- (<> oacc) <$> GM.unsafeRead tree ol'; if p oacc' then do { let { !ol'' = ol' + 1}; if (ol'' .&. (-ol'')) /= ol'' then oloop oacc' ol'' else return $! n} else do { fix (\ iloop !iacc !il -> do { if il < n then do { let { il' = 2 * il}; iacc' <- (<> iacc) <$> GM.unsafeRead tree il'; if p iacc' then iloop iacc' (il' + 1) else iloop iacc il'} else return $! il - n}) oacc ol'}}) mempty (l + n)}
{-# INLINE maxRightSegTree #-}
minLeftSegTree :: (Monoid a, PrimMonad m, GM.MVector mv a) => SegTree mv (PrimState m) a -> Int -> (a -> Bool) -> m Int
minLeftSegTree segtree r p = do { let { tree = getSegTree segtree}; let { !n = unsafeShiftR (GM.length tree) 1}; fix (\ oloop !oacc !or -> do { let { or' = fix (\ loop !r -> if r > 1 && r .&. 1 == 1 then loop (unsafeShiftR r 1) else r) or}; oacc' <- (<> oacc) <$> GM.unsafeRead tree or'; if p oacc' then do { if (or' .&. (-or')) /= or' then oloop oacc' or' else return 0} else do { fix (\ iloop !iacc !ir -> do { if ir < n then do { let { ir' = 2 * ir + 1}; iacc' <- (<> iacc) <$> GM.unsafeRead tree ir'; if p iacc' then iloop iacc' (ir' - 1) else iloop iacc ir'} else return $! ir + 1 - n}) oacc or'}}) mempty (r + n)}
{-# INLINE minLeftSegTree #-}
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x | x > 1 = unsafeCoerce @Word @Int $ unsafeShiftR (complement zeroBits) (countLeadingZeros (x - 1)) + 1 | otherwise = 1
