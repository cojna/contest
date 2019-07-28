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
import qualified Data.Primitive.ByteArray    as BA
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
    s <- C.getLine
    t <- C.getLine
    print $ solve t s

solve :: C.ByteString -> C.ByteString -> Int
solve pat text
    | lenT < lenS = solve pat $ C.concat $ replicate (div (lenS + lenT - 1) lenT) text
    | otherwise = solve' lenS lenT $ zAlgorithm $ C.concat [pat, "$", text, text]
  where
    !lenS = C.length pat
    !lenT = C.length text

solve' lenS lenT z = runST $ do
    uf <- newUnionFind lenT
    merged <- U.mapM (\(src, dst) -> uniteM uf src dst) edges
    if U.and merged
    then U.foldl' max 0 . U.map (subtract 1.negate) . U.filter (<0)
            <$> runUnionFind uf
    else return (-1)
  where
    n = lenT
    edges = U.map (\(i, _) -> (i, rem (i + lenS) n))
        . U.filter ((>=lenS).snd)
        . U.indexed
        $ U.slice (lenS + 1) lenT z

zAlgorithm :: B.ByteString -> U.Vector Int
zAlgorithm bs = U.create $ do
    let n = B.length bs
    z <- UM.replicate n 0
    zbox <- newZBox
    U.forM_ (U.tail $ U.generate n id) $ \i -> do
        r <- readR zbox
        if i > r
        then do
            writeL zbox i
            writeR zbox i
            extendR bs zbox >>= UM.unsafeWrite z i
        else do
            l <- readL zbox
            let k = i - l
            zk <- UM.unsafeRead z k
            if zk < r - i + 1
            then do
                UM.unsafeWrite z i zk
            else do
                writeL zbox i
                extendR bs zbox >>= UM.unsafeWrite z i
    return z

extendR :: (PrimMonad m) => B.ByteString -> ZBox (PrimState m) -> m Int
extendR bs zbox = do
    l <- readL zbox
    r <- readR zbox
    let !n = B.length bs
    let r' = flip fix r $ \loop !i ->
            if i < n && B.unsafeIndex bs (i - l) == B.unsafeIndex bs i
            then loop (i + 1)
            else i
    writeR zbox (r' - 1)
    return $! r' - l
{-# INLINE extendR #-}

newtype ZBox s = ZBox { unZBox :: BA.MutableByteArray s }

newZBox :: (PrimMonad m) => m (ZBox (PrimState m))
newZBox = do
    zbox <- ZBox <$> BA.newByteArray (sizeOf (undefined :: Int) * 2)
    writeL zbox 0
    writeR zbox 0
    return zbox

readL :: (PrimMonad m) => ZBox (PrimState m) -> m Int
readL zbox = BA.readByteArray (coerce zbox) 0

readR:: (PrimMonad m) => ZBox (PrimState m) -> m Int
readR zbox = BA.readByteArray (coerce zbox) 1

writeL :: (PrimMonad m) => ZBox (PrimState m) -> Int -> m ()
writeL zbox = BA.writeByteArray (coerce zbox) 0

writeR :: (PrimMonad m) => ZBox (PrimState m) -> Int -> m ()
writeR zbox = BA.writeByteArray (coerce zbox) 1

-------------------------------------------------------------------------------

newtype UnionFind s = UF { internalUF :: UM.MVector s Int }

runUnionFind :: PrimMonad m => UnionFind (PrimState m) -> m (U.Vector Int)
runUnionFind = U.unsafeFreeze . internalUF

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}

findM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findM uf x = go x return
  where
    go !x k = do
        px <- UM.unsafeRead (internalUF uf) x
        if px < 0
        then k x
        else go px $ \ppx -> do
            UM.unsafeWrite (internalUF uf) x ppx
            k ppx
{-# INLINE findM #-}

sizeM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeM uf = fix $ \loop x -> do
    px <- UM.unsafeRead (internalUF uf) x
    if px < 0
    then return $! negate px
    else loop px
{-# INLINE sizeM #-}

uniteM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
uniteM uf x y = do
    px <- findM uf x
    py <- findM uf y
    if px == py
    then return False
    else do
        rx <- UM.unsafeRead (internalUF uf) px
        ry <- UM.unsafeRead (internalUF uf) py
        if rx < ry
        then do
            UM.unsafeModify (internalUF uf) (+ry) px
            UM.unsafeWrite  (internalUF uf) py px
        else do
            UM.unsafeModify (internalUF uf) (+rx) py
            UM.unsafeWrite  (internalUF uf) px py
        return True
{-# INLINE uniteM #-}

equivM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivM uf x y = (==) `liftM` findM uf x `ap` findM uf y
{-# INLINE equivM #-}

-- | O(n)
countGroupM :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupM uf = U.length . U.filter (<0) <$> runUnionFind uf
{-# INLINE countGroupM #-}

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