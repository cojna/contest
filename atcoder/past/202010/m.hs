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
main = do
    [n, q] <- map read.words <$> getLine
    (es, qs) <- bimap
            (U.map (bimap (subtract 1) (subtract 1)) . tuples2N (n-1))
            (U.map (\(u, v, c) -> (u - 1, v - 1, fromIntegral c)) . tuples3N q)
        . U.splitAt (2*(n-1))
        . U.unfoldrN (2*(n-1)+3*q) (runParser int)
        <$> C.getContents
    putStr.unlines.map show.U.toList $ solve n q es qs

instance MonoidAction (Max (Int, Word)) (Max (Int, Word)) where
    appMonoid = (<>)
    {-# INLINE appMonoid #-}

solve :: Int -> Int -> U.Vector (Int, Int) -> U.Vector (Int, Int, Word) -> U.Vector Word
solve n q es qs = runST $ do
    seg <- newSegTree @(Max (Int, Word)) @(Max (Int, Word)) n
    flip U.imapM qs $ \qi (u, v, c) -> do
        forM_ (pathHLD hld u v) $ \(l, r) -> do
            appFromTo seg l r $ Max (qi, c)
    U.forM es $ \(a, b) -> do
        if parentHLD hld U.! a == b
        then snd . getMax <$> readSegTree seg (indexHLD hld U.! a)
        else snd . getMax <$> readSegTree seg (indexHLD hld U.! b)
  where
    root = 0
    !hld = buildHLD root $ buildUndirectedGraph n es

type HLDIndex = Int

data HLD = HLD
    { indexHLD :: U.Vector HLDIndex
    , parentHLD :: U.Vector Vertex
    , pathHeadHLD :: U.Vector Vertex
    } deriving (Show)

lcaHLD :: HLD -> Vertex -> Vertex -> Vertex
lcaHLD HLD{..} u v = go u v
  where
    go !x !y
        | ix > iy = go y x
        | otherwise =
            let !hx = U.unsafeIndex pathHeadHLD x
                !hy = U.unsafeIndex pathHeadHLD y
            in if hx /= hy
               then go x $ U.unsafeIndex parentHLD hy
               else x
      where
        !ix = U.unsafeIndex indexHLD x
        !iy = U.unsafeIndex indexHLD y

pathHLD :: HLD -> Vertex -> Vertex -> [(HLDIndex, HLDIndex)]
pathHLD HLD{..} = go
  where
    go !x !y
        | ix > iy = go y x
        | hx /= hy =
            let !ihy = U.unsafeIndex indexHLD hy
                !iy' = iy + 1
            in (ihy, iy') : go x (U.unsafeIndex parentHLD hy)
        | ix == iy = []
        | otherwise =
            let !ix' = ix + 1
                !iy' = iy + 1
            in [(ix', iy')]
      where
        !ix = U.unsafeIndex indexHLD x
        !iy = U.unsafeIndex indexHLD y
        hx = U.unsafeIndex pathHeadHLD x
        hy = U.unsafeIndex pathHeadHLD y


buildHLD :: Vertex -> SparseGraph w -> HLD
buildHLD root gr@CSR{..} | numEdgesCSR == 2 * (numVerticesCSR - 1) = runST $ do
    mindexHLD <- UM.unsafeNew numVerticesCSR
    mparentHLD <- UM.replicate numVerticesCSR nothing
    mpathHeadHLD <- UM.replicate numVerticesCSR nothing

    madjacent <- U.thaw adjacentCSR
    void $ fix (\dfs pv v -> do
        UM.write mparentHLD v pv
        (size, (_, heavyId)) <- U.foldM' (\(!sz, !mm) (ei, nv) -> do
            sz' <- dfs v nv
            return (sz + sz', max mm (sz', ei))
            ) (1, (0, nothing))
            . U.filter ((/=pv) . snd) $ gr `iadj` v
        when (heavyId /= nothing) $ do
            UM.swap madjacent heavyId (offsetCSR U.! v)
        return size
        ) nothing root
    void $ fix (\dfs i h pv v -> do
        UM.write mindexHLD v i
        UM.write mpathHeadHLD v h
        let o = offsetCSR U.! v
        nv0 <- UM.read madjacent o
        acc0 <- if nv0 /= pv then dfs (i + 1) h v nv0 else pure i
        MS.foldM' (\acc j -> do
            nv <- UM.read madjacent j
            if nv /= pv
            then dfs (acc + 1) nv v nv
            else pure acc
            ) acc0
            $ stream (o + 1) (offsetCSR U.! (v + 1))
        ) 0 root nothing root

    HLD <$> U.unsafeFreeze mindexHLD
        <*> U.unsafeFreeze mparentHLD
        <*> U.unsafeFreeze mpathHeadHLD
  where
    nothing = -1




tuples2 :: (G.Vector v a, G.Vector v (a, a)) => v a -> v (a, a)
tuples2 = G.unfoldr $ \v ->
  case G.length v >= 2 of
    True -> let !x = G.unsafeIndex v 0
                !y = G.unsafeIndex v 1
            in Just ((x, y), G.drop 2 v)
    False -> Nothing

tuples2N :: (G.Vector v a, G.Vector v (a, a)) => Int -> v a -> v (a, a)
tuples2N n = G.unfoldrN n $ \v ->
    let !x = G.unsafeIndex v 0
        !y = G.unsafeIndex v 1
    in Just ((x, y), G.drop 2 v)

tuples3 :: (G.Vector v a, G.Vector v (a, a, a)) => v a -> v (a, a, a)
tuples3 = G.unfoldr $ \v ->
  case G.length v >= 3 of
    True -> let !x = G.unsafeIndex v 0
                !y = G.unsafeIndex v 1
                !z = G.unsafeIndex v 2
            in Just ((x, y, z), G.drop 3 v)
    False -> Nothing

tuples3N :: (G.Vector v a, G.Vector v (a, a, a)) => Int -> v a -> v (a, a, a)
tuples3N n = G.unfoldrN n $ \v ->
    let !x = G.unsafeIndex v 0
        !y = G.unsafeIndex v 1
        !z = G.unsafeIndex v 2
    in Just ((x, y, z), G.drop 3 v)

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
-- Data.SegTree
-------------------------------------------------------------------------------
class (Monoid f) => MonoidAction f a where { appMonoid :: f -> a -> a}
instance MonoidAction () m where { appMonoid = flip const; {-# INLINE appMonoid #-}}
instance MonoidAction (Sum Int) (Min Int) where { appMonoid (Sum x) (Min y) | y /= maxBound = Min (x + y) | otherwise = Min y; {-# INLINE appMonoid #-}}
instance MonoidAction (Sum Int) (Max Int) where { appMonoid (Sum x) (Max y) | y /= minBound = Max (x + y) | otherwise = Max y; {-# INLINE appMonoid #-}}
instance MonoidAction (Sum Int) (Sum Int, Sum Int) where { appMonoid (Sum x) (Sum y, Sum size) = (Sum (y + x * size), Sum size)}
instance MonoidAction (Product Int) (Sum Int) where { appMonoid = coerce ((*) :: Int -> Int -> Int); {-# INLINE appMonoid #-}}
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
mappendFromTo st l0 r0 = do { let { !l = l0 + sizeSegTree st; !r = r0 + sizeSegTree st}; rev1 (heightSegTree st) $ \ i -> do { when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do { pushSegTree st (unsafeShiftR l i)}; when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do { pushSegTree st (unsafeShiftR r i)}}; fix (\ loop !accL !accR !l' !r' -> do { if l' < r' then do { !accL' <- if l' .&. 1 == 1 then (accL <>) <$!> UM.unsafeRead (getSegTree st) l' else return accL; !accR' <- if r' .&. 1 == 1 then (<> accR) <$!> UM.unsafeRead (getSegTree st) (r' - 1) else return accR; loop accL' accR' (unsafeShiftR (l' + l' .&. 1) 1) (unsafeShiftR (r' - r' .&. 1) 1)} else return $! accL <> accR}) mempty mempty l r}
{-# INLINE mappendFromTo #-}
mappendTo :: (MonoidAction f a, Monoid a, U.Unbox f, U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> Int -> m a
mappendTo st = mappendFromTo st 0
{-# INLINE mappendTo #-}
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
-------------------------------------------------------------------------------
-- Data.VecQueue
-------------------------------------------------------------------------------
data VecQueue s a = VecQueue{intVarsVQ :: !(UM.MVector s Int), internalVecQueue :: !(UM.MVector s a)}
_dequeueCount :: Int
_dequeueCount = 0
{-# INLINE _dequeueCount #-}
_enqueueCount :: Int
_enqueueCount = 1
{-# INLINE _enqueueCount #-}
newVecQueue :: (PrimMonad m, UM.Unbox a) => Int -> m (VecQueue (PrimState m) a)
newVecQueue n = VecQueue <$> UM.replicate 2 0 <*> UM.unsafeNew n
defaultVecQueueSize :: Int
defaultVecQueueSize = 1024 * 1024
lengthVQ :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m Int
lengthVQ (VecQueue info _) = (-) <$> UM.unsafeRead info _enqueueCount <*> UM.unsafeRead info _dequeueCount
{-# INLINE lengthVQ #-}
dequeueVQ :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (Maybe a)
dequeueVQ (VecQueue info q) = do { f <- UM.unsafeRead info _dequeueCount; r <- UM.unsafeRead info _enqueueCount; if f < r then do { UM.unsafeWrite info _dequeueCount (f + 1); pure <$> UM.unsafeRead q f} else return Nothing}
{-# INLINE dequeueVQ #-}
enqueueVQ :: (PrimMonad m, UM.Unbox a) => a -> VecQueue (PrimState m) a -> m ()
enqueueVQ x (VecQueue info q) = do { r <- UM.unsafeRead info _enqueueCount; UM.unsafeWrite q r x; UM.unsafeWrite info _enqueueCount (r + 1)}
{-# INLINE enqueueVQ #-}
enqueuesVQ :: (PrimMonad m, UM.Unbox a) => U.Vector a -> VecQueue (PrimState m) a -> m ()
enqueuesVQ vec (VecQueue info q) = do { r <- UM.unsafeRead info _enqueueCount; UM.unsafeWrite info _enqueueCount (r + U.length vec); U.unsafeCopy (UM.unsafeSlice r (U.length vec) q) vec}
{-# INLINE enqueuesVQ #-}
clearVQ :: (UM.Unbox a, PrimMonad m) => VecQueue (PrimState m) a -> m ()
clearVQ (VecQueue info _) = do { UM.unsafeWrite info _dequeueCount 0; UM.unsafeWrite info _enqueueCount 0}
freezeVecQueue :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (U.Vector a)
freezeVecQueue (VecQueue info q) = do { f <- UM.unsafeRead info _dequeueCount; r <- UM.unsafeRead info _enqueueCount; U.unsafeFreeze $ UM.unsafeSlice f (r - f) q}
-------------------------------------------------------------------------------
-- Data.Graph.Sparse
-------------------------------------------------------------------------------
type Vertex = Int
type Edge = (Vertex, Vertex)
type EdgeWith w = (Vertex, Vertex, w)
type EdgeId = Int
data SparseGraph w = CSR{numVerticesCSR :: !Int, numEdgesCSR :: !Int, offsetCSR :: !(U.Vector Int), adjacentCSR :: !(U.Vector Vertex), edgeCtxCSR :: !(U.Vector w)}
data SparseGraphBuilder s w = SparseGraphBuilder{numVerticesSGB :: !Int, queueSGB :: VecQueue s (EdgeWith w), outDegSGB :: UM.MVector s Int}
buildSparseGraph :: (U.Unbox w) => Int -> (forall s . SparseGraphBuilder s w -> ST s ()) -> SparseGraph w
buildSparseGraph numVerticesCSR run = runST $ do { queueSGB <- newVecQueue (1024 * 1024); outDegSGB <- UM.replicate numVerticesCSR 0; run SparseGraphBuilder{numVerticesSGB = numVerticesCSR, ..}; numEdgesCSR <- lengthVQ queueSGB; offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDegSGB; moffset <- U.thaw offsetCSR; madj <- UM.unsafeNew numEdgesCSR; mectx <- UM.unsafeNew numEdgesCSR; edges <- freezeVecQueue queueSGB; U.forM_ edges $ \ (src, dst, w) -> do { pos <- UM.unsafeRead moffset src; UM.unsafeWrite moffset src (pos + 1); UM.unsafeWrite madj pos dst; UM.unsafeWrite mectx pos w}; adjacentCSR <- U.unsafeFreeze madj; edgeCtxCSR <- U.unsafeFreeze mectx; return CSR{..}}
{-# INLINE buildSparseGraph #-}
addDirectedEdge :: (U.Unbox w, PrimMonad m) => SparseGraphBuilder (PrimState m) w -> EdgeWith w -> m ()
addDirectedEdge SparseGraphBuilder{..} (src, dst, w) = do { enqueueVQ (src, dst, w) queueSGB; UM.unsafeModify outDegSGB (+ 1) src}
{-# INLINE addDirectedEdge #-}
addUndirectedEdge :: (U.Unbox w, PrimMonad m) => SparseGraphBuilder (PrimState m) w -> EdgeWith w -> m ()
addUndirectedEdge SparseGraphBuilder{..} (src, dst, w) = do { enqueueVQ (src, dst, w) queueSGB; enqueueVQ (dst, src, w) queueSGB; UM.unsafeModify outDegSGB (+ 1) src; UM.unsafeModify outDegSGB (+ 1) dst}
{-# INLINE addUndirectedEdge #-}
addDirectedEdge_ :: (PrimMonad m) => SparseGraphBuilder (PrimState m) () -> Edge -> m ()
addDirectedEdge_ SparseGraphBuilder{..} (src, dst) = do { enqueueVQ (src, dst, ()) queueSGB; UM.unsafeModify outDegSGB (+ 1) src}
{-# INLINE addDirectedEdge_ #-}
addUndirectedEdge_ :: (PrimMonad m) => SparseGraphBuilder (PrimState m) () -> Edge -> m ()
addUndirectedEdge_ SparseGraphBuilder{..} (src, dst) = do { enqueueVQ (src, dst, ()) queueSGB; enqueueVQ (dst, src, ()) queueSGB; UM.unsafeModify outDegSGB (+ 1) src; UM.unsafeModify outDegSGB (+ 1) dst}
{-# INLINE addUndirectedEdge_ #-}
buildDirectedGraph :: Int -> U.Vector Edge -> SparseGraph ()
buildDirectedGraph numVerticesCSR edges = buildSparseGraph numVerticesCSR $ \ builder -> do { U.mapM_ (addDirectedEdge_ builder) edges}
buildUndirectedGraph :: Int -> U.Vector Edge -> SparseGraph ()
buildUndirectedGraph numVerticesCSR edges = buildSparseGraph numVerticesCSR $ \ builder -> do { U.mapM_ (addUndirectedEdge_ builder) edges}
buildDirectedGraphW :: (U.Unbox w) => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildDirectedGraphW numVerticesCSR edges = buildSparseGraph numVerticesCSR $ \ builder -> do { U.mapM_ (addDirectedEdge builder) edges}
buildUndirectedGraphW :: (U.Unbox w) => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildUndirectedGraphW numVerticesCSR edges = buildSparseGraph numVerticesCSR $ \ builder -> do { U.mapM_ (addUndirectedEdge builder) edges}
adj :: SparseGraph w -> Vertex -> U.Vector Vertex
adj CSR{..} v = U.unsafeSlice o (o' - o) adjacentCSR where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE adj #-}
iadj :: SparseGraph w -> Vertex -> U.Vector (EdgeId, Vertex)
iadj CSR{..} v = U.imap ((,) . (+ o)) $ U.unsafeSlice o (o' - o) adjacentCSR where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE iadj #-}
adjW :: (U.Unbox w) => SparseGraph w -> Vertex -> U.Vector (Vertex, w)
adjW CSR{..} v = U.zip (U.unsafeSlice o (o' - o) adjacentCSR) (U.unsafeSlice o (o' - o) edgeCtxCSR) where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE adjW #-}
iadjW :: (U.Unbox w) => SparseGraph w -> Vertex -> U.Vector (EdgeId, Vertex, w)
iadjW CSR{..} v = U.izipWith (\ i u w -> (i + o, u, w)) (U.unsafeSlice o (o' - o) adjacentCSR) (U.unsafeSlice o (o' - o) edgeCtxCSR) where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE iadjW #-}
outEdges :: SparseGraph w -> Vertex -> U.Vector EdgeId
outEdges CSR{..} v = U.generate (o' - o) (+ o) where { o = U.unsafeIndex offsetCSR v; o' = U.unsafeIndex offsetCSR (v + 1)}
{-# INLINE outEdges #-}
outDegree :: SparseGraph w -> Vertex -> Int
outDegree CSR{..} v = U.unsafeIndex offsetCSR (v + 1) - U.unsafeIndex offsetCSR v
{-# INLINE outDegree #-}
outDegrees :: SparseGraph w -> U.Vector Int
outDegrees CSR{..} = U.zipWith (-) offsetCSR $ U.tail offsetCSR
{-# INLINE outDegrees #-}
