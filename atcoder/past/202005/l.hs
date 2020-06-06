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
import           Data.Proxy
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
import           GHC.TypeLits
import qualified System.IO                     as IO
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = do
    n <- readLn :: IO Int
    tss <- V.replicateM n $ do
        U.tail.U.unfoldr (runParser int) <$> C.getLine
    m <- readLn
    qs <- U.unfoldrN m (runParser int) <$> C.getLine
    putStr.unlines.map show.U.toList $ solve n tss qs

instance {-# OVERLAPPING #-} Semigroup (Int, Int) where
    x <> y = max x y
instance {-# OVERLAPPING #-} Monoid (Int, Int) where
    mempty = (minBound, minBound)

solve :: Int -> V.Vector (U.Vector Int) -> U.Vector Int  -> U.Vector Int
solve n tss qs0 = runST $ do
    seg <- buildSegTree (U.replicate (2 * n) mempty)
    front <- UM.replicate n (2 :: Int)
    let pop i = do
            let l = U.length $ tss V.! i
            fi <- UM.read front i
            UM.modify front (+1) i
            if fi < l
            then pure . Just $! tss V.! i U.! fi
            else pure $! Nothing
    flip V.imapM_ tss $ \i ts -> do
        writeSegTree seg i (U.head ts, i)
        when (U.length ts >= 2) $ do
            writeSegTree seg (i + n) (ts U.! 1, i + n)
    U.forM qs0 $ \case
        1 -> do
            (res, i) <- mappendFromTo seg 0 n
            (next, _) <- mappendFromTo seg (n + i) (n + i + 1)
            writeSegTree seg i (next, i)
            pop i >>= \case
                Just x -> writeSegTree seg (n + i) (x, n + i)
                Nothing -> writeSegTree seg (n + i) mempty
            pure res
        2 -> do
            (res, i) <- mappendFromTo seg 0 (2 * n)
            when (i < n) $ do
                (next, _) <- mappendFromTo seg (n + i) (n + i + 1)
                writeSegTree seg i (next, i)
            pop (rem i n) >>= \case
                Just x -> writeSegTree seg (n + rem i n) (x, n + rem i n)
                Nothing -> writeSegTree seg (n + rem i n) mempty
            pure res

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
infixl 8 .<<., .>>., .>>>.
infixl 6 .^.
(.<<.) :: (Bits i) => i -> Int -> i
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}
(.>>.) :: (Bits i) => i -> Int -> i
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}
(.>>>.) :: Int -> Int -> Int
(I# x#) .>>>. (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE (.>>>.) #-}
(.^.) :: (Bits i) => i -> i -> i
(.^.) = xor
{-# INLINE (.^.) #-}
-------------------------------------------------------------------------------
-- Data.SegTree
-------------------------------------------------------------------------------
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x | w > 1 = fromIntegral $ unsafeShiftR (maxBound :: Word) (countLeadingZeros (w - 1)) + 1 | otherwise = 1 where { w :: Word; w = fromIntegral x}
newtype SegTree m a = SegTree{getSegTree :: UM.MVector m a}
buildSegTree :: (Monoid a, U.Unbox a, PrimMonad m) => U.Vector a -> m (SegTree (PrimState m) a)
buildSegTree vec = do { let { n = extendToPowerOfTwo $ U.length vec}; tree <- UM.replicate (2 * n) mempty; U.unsafeCopy (UM.unsafeSlice n (U.length vec) tree) vec; rev (n - 1) $ \ i -> do { x <- mappend <$> UM.unsafeRead tree (i .<<. 1) <*> UM.unsafeRead tree (i .<<. 1 .|. 1); UM.unsafeWrite tree i x}; return $ SegTree tree}
writeSegTree :: (Monoid a, U.Unbox a, PrimMonad m) => SegTree (PrimState m) a -> Int -> a -> m ()
writeSegTree segtree k v = do { let { tree = getSegTree segtree}; let { n = UM.length tree .>>. 1}; UM.unsafeWrite tree (k + n) v; flip fix (k + n) $ \ loop !i -> when (i > 1) $ do { x <- mappend <$> UM.unsafeRead tree i <*> UM.unsafeRead tree (i .^. 1); UM.unsafeWrite tree (i .>>. 1) x; loop $ unsafeShiftR i 1}}
mappendFromTo :: (Monoid a, U.Unbox a, PrimMonad m) => SegTree (PrimState m) a -> Int -> Int -> m a
mappendFromTo segtree l r = do { let { tree = getSegTree segtree}; let { n = UM.length tree .>>. 1}; let { stepL l | l .&. 1 == 1 = \ acc -> mappend acc <$> UM.unsafeRead tree l | otherwise = return; stepR r | r .&. 1 == 1 = \ acc -> mappend acc <$> UM.unsafeRead tree (r - 1) | otherwise = return; go l r k | l < r = go ((l + l .&. 1) .>>. 1) ((r - r .&. 1) .>>. 1) $ stepL l >=> (stepR r >=> k) | otherwise = k}; go (n + l) (n + r) return mempty}
