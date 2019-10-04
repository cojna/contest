{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

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
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
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
    xs <- U.unfoldrN n parseInt <$> C.getLine
    print $ solve n xs

solve :: Int -> U.Vector Int -> Int
solve n xs = runST $ do
    ft <- newFenwickTree (n + 1) :: ST s (FenwickTree s Int)
    vars <- UM.replicate 1 (0 :: Int)
    let _res = 0
    addAt (rxs U.! n) 1 ft
    rev n $ \i -> do
        let pos = rxs U.! i
        freq <- sumTo pos ft
        prev' <- findMaxIndexLT (freq - 1) ft
        prev <- findMaxIndexLT freq ft
        next <- findMaxIndexLT (freq + 1) ft
        next' <- findMaxIndexLT (freq + 2) ft
        let resL = (prev - prev') * (next - pos)
        let resR = (pos - prev) * (next' - next)
        let resi = (resL + resR) * i
        UM.unsafeModify vars (+resi) _res
        addAt pos 1 ft
    UM.unsafeRead vars _res
  where
    rxs = U.accumulate (flip const) (U.replicate (n + 1) 0)
        $ U.imap (flip (,)) xs

-------------------------------------------------------------------------------
rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rep #-}
{-# INLINE rev #-}

lowerBoundM :: (Integral i, Monad m) => i -> i -> (i -> m Bool) -> m i
lowerBoundM low high p = go low high
  where
    go !low !high
        | high <= low = return high
        | otherwise = do
            pmid <- p mid
            if pmid
            then go low mid
            else go (mid + 1) high
      where
        h = toInteger high
        l = toInteger low
        mid = fromIntegral $ l + div (h - l) 2

{-# INLINE lowerBoundM #-}

upperBoundM :: (Integral i, Monad m) => i -> i -> (i -> m Bool) -> m i
upperBoundM low high p = do
    phigh <- p high
    if phigh
    then return high
    else subtract 1 <$> lowerBoundM low high (fmap not.p)
{-# INLINE upperBoundM #-}


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


newtype FenwickTree s a = FenwickTree {getFenwickTree :: UM.MVector s a}

newFenwickTree :: (PrimMonad m, U.Unbox a, Num a)
    => Int -> m (FenwickTree (PrimState m) a)
newFenwickTree n = FenwickTree <$> UM.replicate n 0

buildFenwickTree :: (PrimMonad m, U.Unbox a, Num a)
    => U.Vector a -> m (FenwickTree (PrimState m) a)
buildFenwickTree vec = do
    let n = U.length vec
    ft <- UM.replicate (n + 1) 0
    U.unsafeCopy (UM.tail ft) vec
    U.forM_ (U.generate n (+1)) $ \i -> do
        let j = i + (i .&. (-i))
        when (j <= n) $ do
            fti <- UM.unsafeRead ft i
            UM.unsafeModify ft (+fti) j
    return $ FenwickTree ft

getFreezeFenwickTree :: (PrimMonad m, U.Unbox a)
    => FenwickTree (PrimState m) a -> m (U.Vector a)
getFreezeFenwickTree (FenwickTree ft) = do
    U.freeze ft


-- | sum [0..k)
sumTo :: (PrimMonad m, U.Unbox a, Num a)
    => Int -> FenwickTree (PrimState m) a -> m a
sumTo k (FenwickTree ft) = go 0 k
  where
    go !acc !i
        | i > 0 = do
            xi <- UM.unsafeRead ft i
            go (acc + xi) (i - (i .&. (-i)))
        | otherwise = return acc
{-# INLINE sumTo #-}

-- | sum [l..r)
sumFromTo :: (PrimMonad m, U.Unbox a, Num a)
    => Int -> Int -> FenwickTree (PrimState m) a -> m a
sumFromTo l r ft = (-) <$> sumTo r ft <*> sumTo l ft
{-# INLINE sumFromTo #-}

addAt :: (PrimMonad m, U.Unbox a, Num a)
    => Int -> a -> FenwickTree (PrimState m) a -> m ()
addAt k v (FenwickTree ft) = flip fix (k + 1) $ \loop !i -> do
    when (i < n) $ do
        UM.unsafeModify ft (+v) i
        loop $ i + (i .&. (-i))
  where
    n = UM.length ft
{-# INLINE addAt #-}

findMaxIndexLT :: (PrimMonad m, U.Unbox a, Num a, Ord a)
    => a -> FenwickTree (PrimState m) a -> m Int
findMaxIndexLT w0 (FenwickTree ft)
    | w0 <= 0 = return (-1)
    | otherwise = go w0 highestOneBit 0
  where
    n = UM.length ft
    highestOneBit = until (>n) (*2) 1 `quot` 2
    go !w !step !i
        | step == 0 = return i
        | otherwise = do
            if i + step < n
            then do
                u <- UM.unsafeRead ft (i + step)
                if u < w
                then go (w - u) (step `unsafeShiftR` 1) (i + step)
                else go w (step `unsafeShiftR` 1) i
            else go w (step `unsafeShiftR` 1) i
{-# INLINE findMaxIndexLT #-}
