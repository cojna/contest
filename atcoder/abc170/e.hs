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
    [n, q] <- map read.words <$> getLine :: IO [Int]
    (xs,ys) <- U.splitAt n.U.unfoldrN (n + q) (runParser $ (,) <$> int <*> int) <$> C.getContents
    putStr.unlines.map show.U.toList $ solve n q xs ys

deriving via (Min Int) instance Semigroup Int
deriving via (Min Int) instance Monoid Int

lim :: Int
lim = 200200

solve :: Int -> Int -> U.Vector (Int, Int) -> U.Vector (Int, Int) -> U.Vector Int
solve n q infs0 qs = runST $ do
    seg <- newSegTree @Int lim
    let chmax k v = do
            pre <- readSegTree seg k
            when (pre == maxBound || v > pre) $ do
                writeSegTree seg k v
    gr <- VM.replicate lim emptyIMS
    p2g <- UM.replicate n (-1)
    let !rates = U.map fst infs0
    flip U.imapM_ infs0 $ \i ((r::Int), (b::Int)) -> do
        UM.write p2g i b
        chmax b r
        s <- VM.read gr b
        VM.write gr b $! insertIMS r s

    U.forM qs $ \(c0, grd) -> do
        let !c = c0 - 1
        grc <- UM.read p2g c
        UM.write p2g c grd

        preC <- VM.read gr grc
        let !postC = deleteIMS (rates U.! c) preC

        preD <- VM.read gr grd
        let !postD = insertIMS (rates U.! c) preD

        case maxViewIMS postC of
            Just (r,_) -> writeSegTree seg grc r
            Nothing -> writeSegTree seg grc maxBound

        chmax grd (rates U.! c)

        VM.write gr grc postC
        VM.write gr grd postD

        mappendAll seg

extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
    | otherwise = 1

newtype SegTree s a = SegTree { getSegTree :: UM.MVector s a }

newSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => Int -> m (SegTree (PrimState m) a)
newSegTree n = SegTree <$> UM.replicate (2 * extendToPowerOfTwo n) mempty

buildSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => U.Vector a -> m (SegTree (PrimState m) a)
buildSegTree vec = do
    let n = extendToPowerOfTwo $ U.length vec
    tree <- UM.replicate (2 * n) mempty
    U.unsafeCopy (UM.unsafeSlice n (U.length vec) tree) vec
    rev (n - 1) $ \i -> do
        x <- mappend
            <$> UM.unsafeRead tree (unsafeShiftL i 1)
            <*> UM.unsafeRead tree (unsafeShiftL i 1 .|. 1)
        UM.unsafeWrite tree i x
    return $ SegTree tree

readSegTree :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> m a
readSegTree segtree k = do
    let tree = getSegTree segtree
    let n = unsafeShiftRL (UM.length tree) 1
    UM.unsafeRead tree (k + n)

writeSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> a -> m ()
writeSegTree segtree k v = do
    let tree = getSegTree segtree
    let n = unsafeShiftRL (UM.length tree) 1
    UM.unsafeWrite tree (k + n) v
    flip fix (k + n) $ \loop !i ->
        when (i > 1) $ do
            x <- mappend
                <$> UM.unsafeRead tree i
                <*> UM.unsafeRead tree (i `xor` 1)
            UM.unsafeWrite tree (unsafeShiftRL i 1) x
            loop $ unsafeShiftR i 1

mappendFromTo
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> Int -> m a
mappendFromTo segtree l r = do
    let tree = getSegTree segtree
    let n = unsafeShiftRL (UM.length tree) 1
    let stepL l
            | l .&. 1 == 1 = \acc ->
                mappend acc <$> UM.unsafeRead tree l
            | otherwise = return

        stepR r
            | r .&. 1 == 1 = \acc ->
                mappend acc <$> UM.unsafeRead tree (r - 1)
            | otherwise = return

        go l r k
            | l < r = go (unsafeShiftRL (l + l .&. 1) 1) (unsafeShiftRL (r - r .&. 1) 1)
                $ stepL l >=> (stepR r >=> k)
            | otherwise = k
    go (n + l) (n + r) return mempty

mappendTo
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> m a
mappendTo segtree = mappendFromTo segtree 0
{-# INLINE mappendTo #-}

mappendAll
    :: (U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> m a
mappendAll segtree = UM.unsafeRead (getSegTree segtree) 1
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
-------------------------------------------------------------------------------
-- Data.IntMultiSet
-------------------------------------------------------------------------------
newtype IntMultiSet = IntMultiSet{getIntMultiSet :: IM.IntMap Int} deriving (Eq)
instance Show IntMultiSet where { show = show . toList}
instance IsList IntMultiSet where { type Item IntMultiSet = Int; fromList = L.foldl' (flip insertIMS) emptyIMS; toList = concatMap (\ (k, x) -> replicate x k) . IM.toList . (coerce :: IntMultiSet -> IM.IntMap Int)}
emptyIMS :: IntMultiSet
emptyIMS = coerce (IM.empty :: IM.IntMap Int)
singletonIMS :: Int -> IntMultiSet
singletonIMS x = coerce $ IM.singleton x (1 :: Int)
insertIMS :: Int -> IntMultiSet -> IntMultiSet
insertIMS x = coerce . IM.insertWith (+) x (1 :: Int) . coerce
deleteIMS :: Int -> IntMultiSet -> IntMultiSet
deleteIMS x = (coerce :: IM.IntMap Int -> IntMultiSet) . IM.update (\ y -> if y > 1 then Just $! y - 1 else Nothing) x . coerce
deleteAllIMS :: Int -> IntMultiSet -> IntMultiSet
deleteAllIMS x = (coerce :: IM.IntMap Int -> IntMultiSet) . IM.delete x . coerce
memberIMS :: Int -> IntMultiSet -> Bool
memberIMS x = IM.member x . (coerce :: IntMultiSet -> IM.IntMap Int)
notMemberIMS :: Int -> IntMultiSet -> Bool
notMemberIMS x = not . memberIMS x
countIMS :: Int -> IntMultiSet -> Int
countIMS key = IM.findWithDefault 0 key . coerce
lookupLTIMS :: Int -> IntMultiSet -> Maybe Int
lookupLTIMS x = fmap fst . IM.lookupLT x . (coerce :: IntMultiSet -> IM.IntMap Int)
lookupGTIMS :: Int -> IntMultiSet -> Maybe Int
lookupGTIMS x = fmap fst . IM.lookupGT x . (coerce :: IntMultiSet -> IM.IntMap Int)
lookupLEIMS :: Int -> IntMultiSet -> Maybe Int
lookupLEIMS x = fmap fst . IM.lookupLE x . (coerce :: IntMultiSet -> IM.IntMap Int)
lookupGEIMS :: Int -> IntMultiSet -> Maybe Int
lookupGEIMS x = fmap fst . IM.lookupGE x . (coerce :: IntMultiSet -> IM.IntMap Int)
nullIMS :: IntMultiSet -> Bool
nullIMS = IM.null . (coerce :: IntMultiSet -> IM.IntMap Int)
sizeIMS :: IntMultiSet -> Int
sizeIMS = IM.foldl' (+) 0 . (coerce :: IntMultiSet -> IM.IntMap Int)
findMinIMS :: IntMultiSet -> Int
findMinIMS = fst . IM.findMin . (coerce :: IntMultiSet -> IM.IntMap Int)
findMaxIMS :: IntMultiSet -> Int
findMaxIMS = fst . IM.findMax . (coerce :: IntMultiSet -> IM.IntMap Int)
deleteMinIMS :: IntMultiSet -> IntMultiSet
deleteMinIMS = coerce . IM.updateMin (\ x -> if x > 1 then Just $! x - 1 else Nothing) . (coerce :: IntMultiSet -> IM.IntMap Int)
deleteMaxIMS :: IntMultiSet -> IntMultiSet
deleteMaxIMS = coerce . IM.updateMax (\ x -> if x > 1 then Just $! x - 1 else Nothing) . (coerce :: IntMultiSet -> IM.IntMap Int)
maxViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
maxViewIMS = maybe Nothing just . IM.maxViewWithKey . (coerce :: IntMultiSet -> IM.IntMap Int) where { just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet); just ((k, x), m) | x > 1 = case IM.insert k (x - 1) m of { m' -> Just (k, coerce m')} | otherwise = Just (k, coerce m)}
minViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
minViewIMS = maybe Nothing just . IM.minViewWithKey . (coerce :: IntMultiSet -> IM.IntMap Int) where { just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet); just ((k, x), m) | x > 1 = case IM.insert k (x - 1) m of { m' -> Just (k, coerce m')} | otherwise = Just (k, coerce m)}
