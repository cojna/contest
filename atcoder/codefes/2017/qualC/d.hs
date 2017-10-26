{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
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
import           Unsafe.Coerce

main :: IO ()
main = B.getLine >>= print . solve

inf :: Int32
inf = 0x3f3f3f3f

solve :: B.ByteString -> Int32
solve bs = res
  where
    cs = U.map(\c->unsafeShiftL 1 $ ord c - ord 'a').U.fromList $ B.unpack bs
    !ps = U.fromList $ 0 : map (shiftL 1) [0..25]
    !cum = U.scanl1 xor cs
    res = runST $ do
        m <- UM.replicate (2^26) inf
        UM.write m (U.head cum) 1
        U.forM_ (U.tail cum) $ \s -> do
            let step acc p = do
                if s == p
                then return $ min acc 1
                else min acc . (+1) <$> UM.unsafeRead m (fromIntegral $ xor s p)
            tmp <- U.foldM' step inf ps
            UM.unsafeModify m (min tmp) (fromIntegral s)
        UM.read m . fromIntegral $ U.last cum

