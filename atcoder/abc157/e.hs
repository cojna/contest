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
    cs <- U.unfoldrN n (runParser $ fmap byteToInt byte) <$> C.getLine
    q <- readLn
    qs <- U.unfoldrN q (runParser queryP) <$> C.getContents
    putStr.unlines.map show.U.toList $ solve cs qs

instance Monoid Word64 where
    mempty = 0
    mappend = (.|.)

byteToInt :: Word8 -> Int
byteToInt = fromIntegral . subtract 97

queryP :: Parser (Int, Int, Int)
queryP = do
    typ <- int
    case typ of
        1 -> do
            !i <- int1
            byte
            !c <- byteToInt <$> byte
            return (1, i, c)
        2 -> do
            !l <- int1
            !r <- int
            return (2, l, r)
        _ -> undefined

solve :: U.Vector Int -> U.Vector (Int, Int, Int) -> U.Vector Int
solve cs qs = runST $ do
    res <- newVecStack (U.length qs)
    seg <- buildSegTree $ U.map (bit :: Int -> Word64) cs

    U.forM_ qs $ \case
        (1, i, c) -> do
            writeSegTree seg i (bit c)
        (2, l, r) -> do
            x <- popCount <$> mappendFromTo seg l r
            pushVS x res

    freezeVecStack res

extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | w > 1 = fromIntegral
        $ unsafeShiftR (maxBound :: Word) (countLeadingZeros (w - 1)) + 1
    | otherwise = 1
  where
    w :: Word
    w = fromIntegral x

newtype SegTree m a = SegTree { getSegTree :: UM.MVector m a }

-- | O(n)
buildSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => U.Vector a -> m (SegTree (PrimState m) a)
buildSegTree vec = do
    let n = extendToPowerOfTwo $ U.length vec
    tree <- UM.replicate (2 * n) mempty
    U.copy (UM.unsafeSlice n (U.length vec) tree) vec
    rev (n - 1) $ \i -> do
        x <- mappend
            <$> UM.unsafeRead tree (i .<<. 1)
            <*> UM.unsafeRead tree (i .<<. 1 .|. 1)
        UM.unsafeWrite tree i x
    return $ SegTree tree

-- | O(log n)
writeSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> a -> m ()
writeSegTree segtree k v = do
    let tree = getSegTree segtree
    let n = UM.length tree .>>. 1
    UM.unsafeWrite tree (k + n) v
    flip fix (k + n) $ \loop !i ->
        when (i > 1) $ do
            x <- mappend
                <$> UM.unsafeRead tree i
                <*> UM.unsafeRead tree (i .^. 1)
            UM.unsafeWrite tree (i .>>. 1) x
            loop $ unsafeShiftR i 1

-- | mappend [l..r)
--
-- O(log n)
mappendFromTo
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> Int -> m a
mappendFromTo segtree l r = do
    let tree = getSegTree segtree
    let n = UM.length tree .>>. 1
    let stepL l
            | l .&. 1 == 1 = \acc ->
                mappend acc <$> UM.unsafeRead tree l
            | otherwise = return

        stepR r
            | r .&. 1 == 1 = \acc ->
                mappend acc <$> UM.unsafeRead tree (r - 1)
            | otherwise = return

        go l r k
            | l < r = go ((l + l .&. 1) .>>. 1) ((r - r .&. 1) .>>. 1)
                $ stepL l >=> (stepR r >=> k)
            | otherwise = k
    go (n + l) (n + r) return mempty

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
-- Data.VecStack
-------------------------------------------------------------------------------
data VecStack s a = VecStack{intVarsVS :: !(UM.MVector s Int), internalVecStack :: !(UM.MVector s a)}
_sizeVS :: Int
_sizeVS = 0
{-# INLINE _sizeVS #-}
newVecStack :: (PrimMonad m, UM.Unbox a) => Int -> m (VecStack (PrimState m) a)
newVecStack n = VecStack <$> UM.replicate 1 0 <*> UM.unsafeNew n
defaultVecStackSize :: Int
defaultVecStackSize = 1024 * 1024
popVS :: (PrimMonad m, UM.Unbox a) => VecStack (PrimState m) a -> m (Maybe a)
popVS (VecStack info s) = do { len <- UM.unsafeRead info _sizeVS; if len > 0 then do { UM.unsafeWrite info _sizeVS (len - 1); pure <$> UM.unsafeRead s (len - 1)} else return Nothing}
{-# INLINE popVS #-}
pushVS :: (PrimMonad m, UM.Unbox a) => a -> VecStack (PrimState m) a -> m ()
pushVS x (VecStack info s) = do { len <- UM.unsafeRead info _sizeVS; UM.unsafeWrite s len x; UM.unsafeWrite info _sizeVS (len + 1)}
{-# INLINE pushVS #-}
pushesVS :: (PrimMonad m, UM.Unbox a) => U.Vector a -> VecStack (PrimState m) a -> m ()
pushesVS vec (VecStack info s) = do { len <- UM.unsafeRead info _sizeVS; UM.unsafeWrite info _sizeVS (len + U.length vec); U.unsafeCopy (UM.unsafeSlice len (U.length vec) s) vec}
{-# INLINE pushesVS #-}
freezeVecStack :: (PrimMonad m, U.Unbox a) => VecStack (PrimState m) a -> m (U.Vector a)
freezeVecStack (VecStack info s) = do { l <- UM.unsafeRead info _sizeVS; U.unsafeFreeze $ UM.take l s}
{-# INLINE freezeVecStack #-}
-------------------------------------------------------------------------------
-- Data.Bits.Utils
-------------------------------------------------------------------------------
infixl 8 .<<., .>>., .>>>.
infixl 6 .^.
(.<<.) :: (Bits i) => i -> Int -> i
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}
(.>>.) :: (Bits i) => i -> Int -> i
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}
(.>>>.) :: Int -> Int -> Int
(I# x#) .>>>. (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE (.>>>.) #-}
(.^.) :: (Bits i) => i -> i -> i
(.^.) = xor
{-# INLINE (.^.) #-}
