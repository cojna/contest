{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances         #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses   #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeFamilies, ViewPatterns #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bifunctor
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
    n <- readLn :: IO Int
    vs <- U.unfoldrN (2*n) (runParser int) <$> C.getContents
    print $ solve n vs

solve :: Int -> U.Vector Int -> Int
solve n vs = runST $ do
    let numV = n + U.length dic
    let numE = 2 * n
    bmb <- newBipartiteMatchingBuilder numV numE
    flip U.imapM (compress dic vs) $ \i k -> do
        addEdgeBMB (unsafeShiftR i 1) (n + k) bmb
    runBipartiteMatching =<< buildBipartiteMatching bmb
  where
    !dic = radixSort64 vs

data BipartiteMatching s = BipartiteMatching
    { numVerticesBM :: !Int
    , matchBM :: !(UM.MVector s Int)
    , usedBM :: !(UM.MVector s Bool)
    , offsetBM :: !(U.Vector Int)
    , adjacentBM :: !(U.Vector Int)
    }

dfsBM :: (PrimMonad m) => Vertex -> (Bool -> m ()) -> BipartiteMatching (PrimState m) -> m ()
dfsBM v k0 BipartiteMatching{..} = dfs v k0
  where
    dfs !v k = UM.unsafeRead usedBM v >>= \case
        True -> k False
        False -> do
            UM.unsafeWrite usedBM v True
            let begin = U.unsafeIndex offsetBM v
            let end = U.unsafeIndex offsetBM (v + 1)
            flip fix begin $ \loop !i -> do
                if i < end
                then do
                    let nv = U.unsafeIndex adjacentBM i
                    mnv <- UM.unsafeRead matchBM nv
                    if mnv == nothing
                    then do
                        UM.unsafeWrite matchBM v nv
                        UM.unsafeWrite matchBM nv v
                        k True
                    else do
                        dfs mnv $ \case
                            True -> do
                                UM.unsafeWrite matchBM v nv
                                UM.unsafeWrite matchBM nv v
                                k True
                            False -> loop (i + 1)
                else k False
{-# INLINE dfsBM #-}

runBipartiteMatching :: (PrimMonad m)
    => BipartiteMatching (PrimState m) -> m Int
runBipartiteMatching bm@BipartiteMatching{..} = do
    res <- UM.replicate 1 0
    updated <- UM.replicate 1 True
    fix $ \loop -> do
        UM.unsafeWrite updated 0 False
        rep numVerticesBM $ \i -> do
            mi <- UM.unsafeRead matchBM i
            when (mi == nothing) $ do
                flip (dfsBM i) bm $ \case
                    True -> do
                        UM.unsafeWrite updated 0 True
                        UM.unsafeModify res (+1) 0
                    False -> return ()
        UM.unsafeRead updated 0 >>= \case
            True -> do
                UM.set usedBM False
                loop
            False -> UM.unsafeRead res 0
{-# INLINE runBipartiteMatching #-}

data BipartiteMatchingBuilder s = BipartiteMatchingBuilder
    { numVerticesBMB :: !Int
    , inDegreeBMB :: UM.MVector s Int
    , edgesBMB :: VecStack s (Vertex, Vertex)
    }

newBipartiteMatchingBuilder :: (PrimMonad m)
    => Int -> Int -> m (BipartiteMatchingBuilder (PrimState m))
newBipartiteMatchingBuilder n m = BipartiteMatchingBuilder n
    <$> UM.replicate n 0
    <*> newVecStack m

addEdgeBMB :: (PrimMonad m)
    => Vertex -> Vertex -> BipartiteMatchingBuilder (PrimState m) -> m ()
addEdgeBMB !src !dst BipartiteMatchingBuilder{..} = do
    UM.unsafeModify inDegreeBMB (+1) src
    push (src, dst) edgesBMB
{-# INLINE addEdgeBMB #-}

buildBipartiteMatching :: (PrimMonad m)
    => BipartiteMatchingBuilder (PrimState m)
    ->  m (BipartiteMatching (PrimState m))
buildBipartiteMatching BipartiteMatchingBuilder{..} = do
    let numVerticesBM = numVerticesBMB
    matchBM <- UM.replicate numVerticesBM nothing
    usedBM <- UM.replicate numVerticesBM False
    offsetBM <- U.scanl' (+) 0 <$> U.unsafeFreeze inDegreeBMB
    madjacentBM <- UM.unsafeNew (U.last offsetBM)
    moffset <- U.thaw offsetBM
    edges <- freezeVecStack edgesBMB
    U.forM_ edges $ \(src, dst) -> do
        srcOffset <- UM.unsafeRead moffset src
        UM.unsafeWrite madjacentBM srcOffset dst
        UM.unsafeWrite moffset src (srcOffset + 1)
    adjacentBM <- U.unsafeFreeze madjacentBM
    return BipartiteMatching{..}


nothing :: Int
nothing = -1

inf :: (Num a) => a
inf = 0x3f3f3f3f3f3f
{-# INLINE inf #-}

type Vertex = Int

freezeVecStack :: (PrimMonad m, U.Unbox a) => VecStack (PrimState m) a -> m (U.Vector a)
freezeVecStack (VecStack info s) = do
    l <- UM.unsafeRead info 0
    U.unsafeFreeze $ UM.take l s

compress :: U.Vector Int -> U.Vector Int -> U.Vector Int
compress dic = U.map f
  where
    f k = lowerBoundInt 0 (U.length dic - 1) $ \i ->
        k <= U.unsafeIndex dic i

uniqSort :: U.Vector Int -> U.Vector Int
uniqSort v =  U.fromListN (U.length v)
    . map head . L.group . U.toList
    $ radixSort64 v

radixSort64 :: U.Vector Int -> U.Vector Int
radixSort64 v = F.foldl' step v [0, 16, 32, 48]
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step v k = U.create $ do
        pref <- U.unsafeThaw
            . U.prescanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate 0x10000 0)
            $ U.map (flip (,) 1 . mask k) v
        res <- UM.unsafeNew $ U.length v
        U.forM_ v $ \x -> do
            let !masked = mask k x
            i <- UM.unsafeRead pref masked
            UM.unsafeWrite pref masked $ i + 1
            UM.unsafeWrite res i x
        return res
{-# INLINE radixSort64 #-}
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
-- Algorithm.BinarySearch.Int
-------------------------------------------------------------------------------
lowerBoundInt :: Int -> Int -> (Int -> Bool) -> Int
lowerBoundInt low high p = assert (p high) $ go low high where { go !low !high | high <= low = high | p mid = go low mid | otherwise = go (mid + 1) high where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE lowerBoundInt #-}
upperBoundInt :: Int -> Int -> (Int -> Bool) -> Int
upperBoundInt low high p | p high = high | otherwise = lowerBoundInt low high (not . p) - 1
{-# INLINE upperBoundInt #-}
-------------------------------------------------------------------------------
-- Data.VecStack
-------------------------------------------------------------------------------
data VecStack s a = VecStack{stackInfo :: !(UM.MVector s Int), stackData :: !(UM.MVector s a)}
newVecStack :: (PrimMonad m, UM.Unbox a) => Int -> m (VecStack (PrimState m) a)
newVecStack n = VecStack <$> UM.replicate 1 0 <*> UM.unsafeNew n
defaultVecStackSize :: Int
defaultVecStackSize = 1024 * 1024
pop :: (PrimMonad m, UM.Unbox a) => VecStack (PrimState m) a -> m (Maybe a)
pop (VecStack info s) = do { len <- UM.unsafeRead info 0; if len > 0 then do { UM.unsafeWrite info 0 (len - 1); pure <$> UM.unsafeRead s (len - 1)} else return Nothing}
{-# INLINE pop #-}
push :: (PrimMonad m, UM.Unbox a) => a -> VecStack (PrimState m) a -> m ()
push x (VecStack info s) = do { len <- UM.unsafeRead info 0; UM.unsafeWrite s len x; UM.unsafeWrite info 0 (len + 1)}
{-# INLINE push #-}
