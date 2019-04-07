{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString             as B
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as BL
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
    [n, q] <- map read.words <$> getLine :: IO [Int]
    cs <- B.getLine
    lrs <- U.unfoldrN q parseInt2 <$> C.getContents
    putNonBlocking
        . B.toLazyByteStringWith (B.safeStrategy smallChunkSize chunkSize) mempty
        . U.foldr (\x b -> B.intDec x <> B.char7 '\n' <> b) mempty
        $ solve n cs lrs

solve :: Int -> B.ByteString -> U.Vector (Int, Int) -> U.Vector Int
solve n cs lrs = U.map (uncurry query) lrs
  where
    table :: U.Vector Int
    !table = U.scanl' (+) 0 . U.generate n $ \i ->
        if  | i > 0 -> fromEnum $ B.unsafeIndex cs (i - 1) == 65 && B.unsafeIndex cs i == 67
            | otherwise -> 0
    {-# NOINLINE table #-}

    query l r = U.unsafeIndex table r - U.unsafeIndex table l

putNonBlocking :: BL.ByteString -> IO ()
putNonBlocking = BL.foldrChunks ((>>).B.hPutNonBlocking IO.stdout) (return ())

smallChunkSize :: Int
smallChunkSize = 32 * 1024 - 2 * sizeOf (undefined :: Int)

chunkSize :: Int
chunkSize = 256 * 1024 - 2 * sizeOf (undefined :: Int)

-------------------------------------------------------------------------------
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