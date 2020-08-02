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

#define MOD 1000000007

main :: IO ()
main = do
    [n, q] <- map read.words <$> getLine
    cs <- U.unfoldrN n (runParser int) <$> C.getLine
    qs <- U.unfoldrN q (runParser $ (,) <$> int1 <*> int) <$> C.getContents
    B.hPutBuilder IO.stdout . U.foldr (\x b -> B.intDec x <> B.char7 '\n' <> b) mempty
        $ solve n q cs qs

solve :: Int -> Int -> U.Vector Int -> U.Vector (Int, Int) -> U.Vector Int
solve n q xs query = runST $ do
    freq <- UM.replicate (n + 1) (0 :: Int)
    UM.write freq (U.head xs) 1
    moAlgorithm
        (\acc i -> do
            let x = U.unsafeIndex xs i
            f <- UM.unsafeRead freq x
            UM.unsafeWrite freq x (f + 1)
            if f == 0
            then return $! acc + 1
            else return $! acc
        )
        (\acc i -> do
            let x = U.unsafeIndex xs i
            f <- UM.unsafeRead freq x
            UM.unsafeWrite freq x (f - 1)
            if f == 1
            then return $! acc - 1
            else return $! acc
        ) 1 query

moAlgorithm
    :: (PrimMonad m, U.Unbox a)
    => (a -> Int -> m a)
    -> (a -> Int -> m a)
    -> a
    -> U.Vector (Int, Int)
    -> m (U.Vector a)
moAlgorithm add remove x0 lrs = do
    result <- UM.unsafeNew (U.length lrs)
    U.foldM' (\(MoState l r acc) (qi, (ql, qr)) -> do
        !addR <- MS.foldM' add acc $ iter r qr
        !removeR <- MS.foldM' remove addR $ iterR qr r
        !addL <- MS.foldM' add removeR $ iterR ql l
        !removeL <- MS.foldM' remove addL $ iter l ql
        UM.unsafeWrite result qi removeL
        return $! MoState ql qr removeL
        ) (MoState 0 1 x0) (moSort lrs)
    U.unsafeFreeze result
{-# INLINE moAlgorithm #-}

data MoState a = MoState !Int !Int !a deriving (Eq)

iter :: (Monad m) => Int -> Int -> MS.Stream m Int
iter l r = MS.Stream step l
  where
    step x
        | x < r = return $ MS.Yield x (x + 1)
        | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] iter #-}

iterR :: (Monad m) => Int -> Int -> MS.Stream m Int
iterR l r = MS.Stream step (r - 1)
  where
    step x
        | x >= l = return $ MS.Yield x (x - 1)
        | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] iterR #-}

moBlockSize :: Int
moBlockSize = 750
{-# INLINE moBlockSize #-}

moSort :: U.Vector (Int, Int) -> U.Vector (Int, (Int, Int))
moSort lrs
    = U.map (\i -> (i, U.unsafeIndex lrs i))
    . U.map moDecode
    . radixSort64
    $ U.imap (\i (l, r) -> moEncode i l r) lrs
{-# INLINE moSort #-}

moEncode :: Int -> Int -> Int -> Word64
moEncode qi l r = unsafeCoerce @Int @Word64
    $ unsafeShiftL l' 40 .|. unsafeShiftL r' 20 .|.  qi
  where
    l' = quot l moBlockSize
    r' | l' .&. 1 == 1 = 0xfffff - r
       | otherwise = r
{-# INLINE moEncode #-}

moDecode :: Word64 -> Int
moDecode = unsafeCoerce @Word64 @Int . (.&. 0xfffff)
{-# INLINE moDecode #-}

radixSort64 :: U.Vector Word64 -> U.Vector Word64
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
