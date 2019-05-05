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
import qualified Data.ByteString.Char8       as C
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
import           Unsafe.Coerce

main :: IO ()
main = do
    [h, w] <- map read.words <$> getLine :: IO [Int]
    m <- U.filter (not.isSpace) . U.unfoldrN (h*w+h) C.uncons <$> C.getContents
    print $ solve h w m

convert :: Char -> Int
convert = (inf *) . fromEnum . (/= '#')

inf :: Int
inf = 0x3f3f3f3f

solve :: Int -> Int -> U.Vector Char -> Int
solve h w (U.map convert -> m) = U.maximum $ U.create $ do
    q <- newQueueM
    U.forM_ (U.findIndices (== 0) m) $ \xy -> do
        enqueueM xy q

    d <- U.unsafeThaw m

    fix $ \loop -> do
        dequeueM q >>= \case
            Just xy -> do
                let (x, y) = unIx xy
                neighbor4 x y $ \nx ny -> do
                    let nxny = ix nx ny
                    when (inGrid nx ny) $ do
                        dnxny <- UM.unsafeRead d nxny
                        when (dnxny == inf) $ do
                            UM.unsafeRead d xy >>= UM.unsafeWrite d nxny . (+1)
                            enqueueM nxny q
                loop
            Nothing -> return ()
    return d
  where
    ix x y = x * w + y
    unIx xy = quotRem xy w
    inGrid x y = 0 <= x && x < h && 0 <= y && y < w

neighbor4 :: (Applicative f) => Int -> Int -> (Int -> Int -> f ()) -> f ()
neighbor4 x y f = f (x - 1) y *> f x (y - 1) *> f x (y + 1) *> f (x + 1) y
{-# INLINE neighbor4 #-}

data VecQueue m a = VecQueue
    { queueInfo :: !(UM.MVector m Int)
    , queueData :: !(UM.MVector m a)
    }

newQueueM :: (PrimMonad m, U.Unbox a) => m (VecQueue (PrimState m) a)
newQueueM = VecQueue <$> UM.replicate 2 0 <*> UM.unsafeNew (1024 * 1024)

dequeueM :: (PrimMonad m, U.Unbox a) => VecQueue (PrimState m) a -> m (Maybe a)
dequeueM (VecQueue info q) = do
    h <- UM.unsafeRead info 0
    t <- UM.unsafeRead info 1
    if h < t
    then do
        UM.unsafeWrite info 0 (h + 1)
        pure <$> UM.unsafeRead q h
    else return Nothing
{-# INLINE dequeueM #-}

enqueueM :: (PrimMonad m, U.Unbox a) => a -> VecQueue (PrimState m) a -> m ()
enqueueM x (VecQueue info q) = do
    t <- UM.unsafeRead info 1
    UM.unsafeWrite q t x
    UM.unsafeWrite info 1 (t + 1)
{-# INLINE enqueueM #-}
