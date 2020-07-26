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
import           Data.Monoid
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
import qualified System.IO                         as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [k, m] <- map read.words <$> getLine
    xs <- U.unfoldrN k (runParser int) <$> C.getLine
    ys <- U.unfoldrN k (runParser int) <$> C.getLine
    print $ solve k m xs ys

solve :: Int -> Int -> U.Vector Int -> U.Vector Int -> Int
solve k m xs ys
    | m <= k = xs U.! (m - 1)
    | otherwise = reifyNat k $ \proxy ->
        let !mat = createSqMat @_ @(XorAnd Int) proxy $ \n mba -> do
                setByteArray @(XorAnd Int) mba 0 (n * n) 0
                U.imapM_ (\i y -> writeByteArray mba i $ XorAnd y) ys
                rep (n - 1) $ \i -> do
                    writeByteArray @(XorAnd Int) mba ((i + 1) * n + i) 1
            !f = viewRowSqMat ((inline (^)) mat (m - 1)) (k - 1)
        in getXorAnd . P.sum $ P.zipWith (*) f (P.map coerce . U.convert $ U.reverse xs)

reifyNat :: (Integral i) => i -> (forall n. KnownNat n => Proxy n -> a) -> a
reifyNat n f = case someNatVal (fromIntegral n) of
    Just (SomeNat proxy) -> f proxy
    Nothing -> error $ "reifyNat: " <> show (fromIntegral n)
{-# INLINE reifyNat #-}

newtype XorAnd a = XorAnd { getXorAnd :: a}
    deriving newtype (Eq, Show, Bits, Prim)

instance (Bits a) => Num (XorAnd a) where
    {-# SPECIALIZE instance Num (XorAnd Int) #-}
    (+) = xor
    {-# INLINE (+) #-}
    (-) = xor
    {-# INLINE (-) #-}
    (*) = (.&.)
    {-# INLINE (*) #-}
    negate = id
    {-# INLINE negate #-}
    abs = id
    {-# INLINE abs #-}
    signum = id
    {-# INLINE signum #-}
    fromInteger x
        | x .&. 1 == 0 = zeroBits
        | otherwise = complement zeroBits
    {-# INLINE fromInteger #-}

data SqMat (n :: Nat) a = SqMat !Int !ByteArray

viewRowSqMat :: (KnownNat n, Prim a, G.Vector v a)
    => SqMat n a -> Int -> v a
viewRowSqMat (SqMat n ba) i
    = G.unstream
    . MB.reVector
    $ MB.fromVector (P.Vector (i * n) n ba)
{-# INLINE viewRowSqMat #-}

viewColSqMat :: (KnownNat n, Prim a, G.Vector v a)
    => SqMat n a -> Int -> v a
viewColSqMat (SqMat n ba) j
    = G.unstream
    . MB.map (indexByteArray ba)
    $ MB.iterateN n (+n) j
{-# INLINE viewColSqMat #-}

createSqMat :: forall n a. (KnownNat n, Prim a)
    => Proxy n -> (forall s . Int -> MutableByteArray s -> ST s ()) -> SqMat n a
createSqMat proxy fill = runST $ do
    let n = fromIntegral $ natVal proxy
    mba <- newByteArray (I# (sizeOf# (undefined :: a)) * n * n)
    fill n mba
    SqMat n <$!> unsafeFreezeByteArray mba
{-# INLINE createSqMat #-}

streamSqMat :: (Prim a, Monad m) => SqMat n a -> MS.Stream m a
streamSqMat (SqMat n ba) = MS.generateM (n * n) $ return . indexByteArray ba
{-# INLINE [1] streamSqMat #-}

unstreamSqMat :: forall n a m. (KnownNat n, Prim a) => MS.Stream Id a -> SqMat n a
unstreamSqMat s = createSqMat Proxy $ \n mba -> do
    MS.mapM_ (\(i, x) -> writeByteArray mba i x)
        $ MS.trans (return . unId) $ MS.indexed s
{-# INLINE [1] unstreamSqMat #-}

{-# RULES
"streamSqMat/unstreamSqMat" forall s.
    streamSqMat (unstreamSqMat s) = MS.trans (return . unId) s
"unstreamSqMat/streamSqMat" forall mat.
    unstreamSqMat (streamSqMat mat) = mat
#-}

liftSqMat0 :: forall n a. (KnownNat n, Num a, Prim a) => a -> SqMat n a
liftSqMat0 x = createSqMat Proxy $ \n mba -> do
    setByteArray mba 0 (n * n) (0 :: a)
    MS.mapM_ (\i -> writeByteArray mba i x) $ MS.iterateN n (+(n+1)) 0
{-# INLINE liftSqMat0 #-}

liftSqMat1 :: (KnownNat n, Prim a) => (a -> a) -> SqMat n a -> SqMat n a
liftSqMat1 f = unstreamSqMat . MS.map f . streamSqMat
{-# INLINE liftSqMat1 #-}

liftSqMat2
    :: (KnownNat n, Prim a)
    => (a -> a -> a) -> SqMat n a -> SqMat n a -> SqMat n a
liftSqMat2 f x y = unstreamSqMat
    $ MS.zipWith f (streamSqMat x) (streamSqMat y)
{-# INLINE liftSqMat2 #-}

mulSqMat :: forall n a.
    (KnownNat n, Num a, Prim a) => SqMat n a -> SqMat n a -> SqMat n a
mulSqMat x y = createSqMat Proxy $ \n mba -> do
    rep n $ \i -> do
        let r = viewRowSqMat x i
        rep n $ \j -> do
            let c = viewColSqMat y j
            writeByteArray @a mba (i * n + j) . P.sum $ P.zipWith (*) r c
{-# INLINE mulSqMat #-}

instance (KnownNat n, Num a, Prim a) => Num (SqMat n a) where
    (+) = liftSqMat2 (+)
    {-# INLINE (+) #-}
    (-) = liftSqMat2 (-)
    {-# INLINE (-) #-}
    (*) = mulSqMat
    {-# INLINE (*) #-}
    negate = liftSqMat1 negate
    {-# INLINE negate #-}
    abs = id
    {-# INLINE abs #-}
    signum = id
    {-# INLINE signum #-}
    fromInteger = liftSqMat0 . fromInteger
    {-# INLINE fromInteger #-}

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
