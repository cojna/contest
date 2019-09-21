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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Foreign                     hiding (void, newArray)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce
--
import Data.Array.Base
import Data.Array.ST (runSTUArray)
import Data.Ix
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
import           Data.Semigroup as Semigroup
#endif

main :: IO ()
main = do
    n <- readLn :: IO Int
    bs <- C.getLine
    print $ solve n bs

solve :: Int -> B.ByteString -> Int
solve n bs = U.maximum . U.generate (n + 1) $ \i -> do
    U.maximum . U.generate (n + 1) $ \j -> do
        if i >= j then 0
        else queryMin rmq i j `min` abs (getSuffixStartPos sa i - getSuffixStartPos sa j)
  where
    !sa = buildSuffixArray bs
    !lcp = buildLCPArray bs sa
    !rmq = buildRMQ $ getLCPArray lcp

-------------------------------------------------------------------------------

floorLog2 :: Int -> Int
floorLog2 x = fromIntegral $ unsafeShiftR y 52 - 1023
  where
    y :: Word64
    y = unsafeCoerce (fromIntegral x :: Double)

#if !MIN_VERSION_base(4,9,0)
newtype Min a = Min {getMin :: a} deriving (Eq, Ord, Show)
instance (Ord a, Bounded a) => Monoid (Min a) where
    mappend = coerce (min :: a -> a -> a)
    {-# INLINE mappend #-}
    mempty = coerce (maxBound :: a)
    {-# INLINE mempty #-}
#endif

type RMQ a = SparseTable Min a

buildRMQ :: (U.Unbox a, Ord a, Bounded a) => U.Vector a -> RMQ a
buildRMQ = buildSparseTable
{-# INLINE buildRMQ #-}

queryMin :: (U.Unbox a, Ord a, Bounded a) => RMQ a -> Int -> Int -> a
queryMin = queryST
{-# INLINE queryMin #-}

newtype SparseTable (f :: * -> *) a = SparseTable
    { getSparseTable :: V.Vector (U.Vector a)
    } deriving (Eq, Show)

buildSparseTable :: forall (f :: * -> *) a .
    (U.Unbox a, Monoid (f a), Coercible (f a) a)
    => U.Vector a -> SparseTable f a
buildSparseTable vec = SparseTable
    . V.scanl' (\acc i -> U.zipWith (coerce op) acc $ U.drop i acc) vec
    $ V.iterateN (floorLog2 $ U.length vec) (*2) 1
  where
    op :: f a -> f a -> f a
    op = mappend

queryST :: forall (f :: * -> *) a .
    (U.Unbox a, Monoid (f a), Coercible (f a) a)
    => SparseTable f a -> Int -> Int -> a
queryST st l r
    | l < r = (coerce op) x y
    | otherwise = error $ "queryST l: " ++ shows l "r: " ++ show r
  where
    op :: f a -> f a -> f a
    op = mappend
    logStep = floorLog2 $ r - l
    row = V.unsafeIndex (getSparseTable st) logStep
    x = U.unsafeIndex row l
    y = U.unsafeIndex row $ r - unsafeShiftL 1 logStep

rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)

newtype LCPArray = LCPArray {getLCPArray :: U.Vector Int} deriving (Show)

newtype SuffixArray = SuffixArray {getSuffixArray :: U.Vector Int} deriving (Show)

getSuffixStartPos :: SuffixArray -> Int -> Int
getSuffixStartPos = U.unsafeIndex . getSuffixArray
{-# INLINE getSuffixStartPos #-}

getSuffix :: SuffixArray -> Int -> B.ByteString -> B.ByteString
getSuffix  = (B.unsafeDrop.) . getSuffixStartPos
{-# INLINE getSuffix #-}

buildSuffixArray :: B.ByteString -> SuffixArray
buildSuffixArray bs = SuffixArray $ U.create $ do
  let !n = B.length bs :: Int
      !countMax = max 0xff (n+1)

  sa    <- UM.replicate (n + 1) (0 :: Int)
  rank  <- UM.replicate (n + 1) (0 :: Int)
  tmp   <- UM.replicate (n + 1) (0 :: Int)
  count <- UM.replicate (countMax + 1) (0 :: Int)

  rep n $ \i-> do
    UM.unsafeWrite sa i i
    UM.unsafeWrite rank i . fromIntegral $ B.unsafeIndex bs i
  UM.unsafeWrite sa n n
  UM.unsafeWrite rank n 1

  forM_ (takeWhile (<=n) $ iterate (*2) 1) $ \k-> do
    -- sort sa
    rep (countMax+1) $ \i-> do
      UM.unsafeWrite count i 0
    rep (n+1) $ \i-> do
      sai <- UM.unsafeRead sa i
      if sai+k<=n then do
        rankik <- UM.unsafeRead rank (sai+k)
        UM.unsafeModify count (+1) rankik
      else UM.unsafeModify count (+1) 0
    rep countMax $ \i-> do
      cnti <- UM.unsafeRead count i
      UM.unsafeModify count (+cnti) (i+1)
    rev (n+1) $ \i-> do
      sai <- UM.unsafeRead sa i
      rankik <- if sai + k <= n then do
                   UM.unsafeRead rank (sai+k)
                else return 0
      j <- subtract 1 <$> UM.unsafeRead count rankik
      UM.unsafeWrite count rankik j
      UM.unsafeRead sa i>>=UM.unsafeWrite tmp j

    rep (countMax+1) $ \i-> do
      UM.unsafeWrite count i 0
    rep (n+1) $ \i-> do
      sai <- UM.unsafeRead tmp i
      ranki <- UM.unsafeRead rank sai
      UM.unsafeModify count (+1) ranki
    rep countMax $ \i-> do
      cnti <- UM.unsafeRead count i
      UM.unsafeModify count (+cnti) (i+1)
    rev (n+1) $ \i-> do
      sai <- UM.unsafeRead tmp i
      ranki <- UM.unsafeRead rank sai
      j <- subtract 1 <$> UM.unsafeRead count ranki
      UM.unsafeWrite count ranki j
      UM.unsafeWrite sa j sai

    -- update rank
    sa0 <- UM.unsafeRead sa 0
    UM.unsafeWrite tmp sa0 1
    rep n $ \i-> do
      sai    <- UM.unsafeRead sa i
      sai1   <- UM.unsafeRead sa (i+1)
      ranki  <- UM.unsafeRead rank sai
      ranki1 <- UM.unsafeRead rank sai1
      if ranki == ranki1 then do
           rankik  <- if sai  + k > n then return (-1)
                      else UM.unsafeRead rank (sai+k)
           ranki1k <- if sai1 + k > n then return (-1)
                      else UM.unsafeRead rank (sai1+k)
           if rankik == ranki1k then do
             UM.unsafeRead tmp sai >>= UM.unsafeWrite tmp sai1
           else do
             UM.unsafeRead tmp sai >>= UM.unsafeWrite tmp sai1 . (+1)
      else do
        UM.unsafeRead tmp sai >>= UM.unsafeWrite tmp sai1 . (+1)

    rep (n+1) $ \i-> do
      UM.unsafeRead tmp i >>= UM.unsafeWrite rank i

  return sa
-------------------------------------------------------------------------------
-- Longest Common Prefix Array

naiveLCP :: B.ByteString -> B.ByteString -> Int
naiveLCP xs ys = go 0
   where
     !n = min (B.length xs) (B.length ys)
     go !i
       | i < n && B.unsafeIndex xs i == B.unsafeIndex ys i = go (i+1)
       | otherwise = i

buildLCPArray :: B.ByteString -> SuffixArray -> LCPArray
buildLCPArray bs sa = LCPArray $ U.create $ do
  let !n = B.length bs
      rank = U.unsafeAccumulate (flip const) (U.generate (n + 1) id)
           . U.imap (flip (,))
           $ getSuffixArray sa
  lcp <- UM.replicate (n + 1) (0 :: Int)
  let go !i !h
        | i < n = do
            let xs = B.unsafeDrop (i + h) bs
            let ys = B.unsafeDrop (getSuffixStartPos sa (U.unsafeIndex rank i - 1) + h) bs
            let h' = h + naiveLCP xs ys
            UM.unsafeWrite lcp (U.unsafeIndex rank i - 1) h'
            go (i + 1) . max 0 $ h' - 1
        | otherwise = return lcp
  go 0 0

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