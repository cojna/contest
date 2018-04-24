{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bits
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8     as B
import           Data.Char
import qualified Data.Foldable             as F
import           Data.Function
import           Data.Int
import qualified Data.List                 as L
import           Data.Monoid
import qualified Data.Traversable          as T
import           Data.Word
import           System.IO
--
import           Foreign.ForeignPtr.Safe
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

main :: IO ()
main = do
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True
    [n, t] <- map read.words <$> getLine
    xs <- _AMunfoldrN n (runStateT parseInt) =<< B.getLine
    lrs <- _AMunfoldrN t (runStateT $ (\(x,y)->(x-1,y-1)) <$> parseInt2) =<< B.getContents
    res <- solve n t xs lrs
    builder <- _AMfoldr' (\x b -> int64Dec x <> char7 '\n' <> b) mempty res
    hPutBuilder stdout builder
    hFlush stdout

limit :: Int
limit = 1000000

solve :: Int -> Int -> CMArray Int -> CMArray (Int, Int) -> IO (CMArray Int64)
solve _ t arr lrs = do
    result <- _AMunsafeNew t
    freq <- _AMreplicate (limit + 1) 0
    let ctx = MoCtx arr lrs result freq
    h <- _AMunsafeRead arr 0
    _AMunsafeWrite freq h 1
    sorted <- moSort t lrs
    void $ _AMfoldl'M (\mos encoded ->
        step ctx mos $ moDecode encoded
      )(MoS 0 0 (fromIntegral h)) sorted
    return result

moSort :: Int -> CMArray (Int, Int) -> IO (CMArray Word64)
moSort m lrs = do
    encoded <- _AMunsafeNew m

    _AMifor_ lrs $ \i (l, r) ->
        _AMunsafeWrite encoded i $ moEncode l r i

    radixSort64 encoded
    return encoded

radixSort64 :: CMArray Word64 -> IO ()
radixSort64 arr0@(CMA o n _) = assert (o == 0) $ do
    buf <- _AMunsafeNew n
    freq <- _AMunsafeNew 0x10000
    void $ foldlM step (arr0, buf, freq) [0, 16, 32, 48]
  where
    step (arr, buf, freq) k = do
        _AMset buf 0
        _AMset freq 0
        _AMfor_ arr $ \x -> do
            let masked = fromIntegral $ unsafeShiftR x k .&. 0xffff
            _AMunsafeModify freq masked (+1)
        for_ [1..0xffff] $ \i -> do
            f <- _AMunsafeRead freq (i - 1)
            _AMunsafeModify freq i (+f)
        _AMrfor_ arr $ \ai -> do
            let masked = fromIntegral $ (ai `unsafeShiftR` k) .&. 0xffff
            j <- subtract 1 <$> _AMunsafeRead freq masked
            _AMunsafeWrite freq masked j
            _AMunsafeWrite buf j ai
        return (buf, arr, freq)

moBlockSize :: Int
moBlockSize = 450
{-# INLINE moBlockSize #-}

moEncode :: Int -> Int -> Int -> Word64
moEncode l r qi = unsafeShiftL l' 40 .|. unsafeShiftL r' 20 .|. fromIntegral qi
  where
    l' = fromIntegral $ quot l moBlockSize
    r' | testBit l' 0 = 0xfffff - fromIntegral r
       | otherwise = fromIntegral r
{-# INLINE moEncode #-}

moDecode :: Word64 -> Int
moDecode bits = fromIntegral $ bits .&. 0xfffff
{-# INLINE moDecode #-}

data MoState = MoS
    { moL :: {-# UNPACK #-} !Int
    , moR :: {-# UNPACK #-} !Int
    , moA :: {-# UNPACK #-} !Int64
    } deriving (Eq, Ord)

data MoContext = MoCtx
    { getArray  :: !(CMArray Int)
    , getQuery  :: !(CMArray (Int, Int))
    , getResult :: !(CMArray Int64)
    , getFreq   :: !(CMArray Int)
    }

step :: MoContext -> MoState -> Int -> IO MoState
step (MoCtx arr query result freq) (MoS l0 r0 acc) qi = do
    let add i = do
            x <- _AMunsafeRead arr i
            f <- _AMunsafeRead freq x
            _AMunsafeWrite freq x (f + 1)
            acc <- _AMunsafeRead result qi
            _AMunsafeWrite result qi $! acc + fromIntegral (f + f + 1) * fromIntegral x
    let remove i = do
            x <- _AMunsafeRead arr i
            f <- _AMunsafeRead freq x
            _AMunsafeWrite freq x (f - 1)
            acc <- _AMunsafeRead result qi
            _AMunsafeWrite result qi $! acc - fromIntegral (f + f - 1) * fromIntegral x
    _AMunsafeWrite result qi acc
    (l, r) <- _AMunsafeRead query qi
    traverse_ add [r0+1..r]
    traverse_ (remove.negate) [-r0.. -(r+1)] -- [r0, r0-1..r+1]
    traverse_ (add.negate)[1-l0..(-l)] -- [l0-1, l0-2..l]
    traverse_ remove [l0..l-1]
    res <- _AMunsafeRead result qi
    return $! MoS l r res

type Parser a = StateT B.ByteString Maybe a

parseInt :: Parser Int
parseInt = StateT $ B.readInt . B.dropWhile isSpace
{-# INLINE parseInt #-}

parseInt2 :: Parser (Int, Int)
parseInt2 = (,) <$> parseInt <*> parseInt
{-# INLINE parseInt2 #-}

traverse_ :: (Monad m) => (Int -> m ()) -> [Int] -> m ()
traverse_ f = foldr((>>).f)(return())
{-# INLINE traverse_ #-}

for_ :: (Monad m) => [Int] -> (Int -> m ()) -> m ()
for_ = flip traverse_
{-# INLINE for_ #-}

foldlM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldlM f a xs=foldr((>=>).flip f)return xs$a
{-# INLINE foldlM #-}

data CMArray a = CMA
    {-# UNPACK #-} !Int            -- ^ offset
    {-# UNPACK #-} !Int            -- ^ length
    {-# UNPACK #-} !(ForeignPtr a) -- ^ array
    deriving (Eq, Show)

_AMlength :: CMArray a -> Int
_AMlength (CMA _ l _) = l

_AMunsafeTail :: CMArray a -> CMArray a
_AMunsafeTail (CMA o l acc) = CMA (o + 1) (l - 1) acc
{-# INLINE _AMunsafeTail #-}

_AMunsafeTake :: Int -> CMArray a -> CMArray a
_AMunsafeTake n (CMA o l acc) = CMA o (min n l) acc
{-# INLINE _AMunsafeTake #-}

_AMunsafeDrop :: Int -> CMArray a -> CMArray a
_AMunsafeDrop n (CMA o l acc) = CMA (o + min l n) (max 0 $ l - n) acc
{-# INLINE _AMunsafeDrop #-}

_AMunsafeNew :: (Storable a) => Int -> IO (CMArray a)
_AMunsafeNew n = do
        arr <-  mallocForeignPtrArray n
        return $! CMA 0 n arr
{-# INLINE _AMunsafeNew #-}

_AMreplicate :: (Storable a) => Int -> a -> IO (CMArray a)
_AMreplicate n x = do
    ptr <- mallocForeignPtrArray n
    withForeignPtr ptr $ \arr -> do
        flip fix 0 $ \loop !i ->
            when (i < n) $ do
                pokeElemOff arr i x
                loop $ i + 1
        return $ CMA 0 n ptr
{-# INLINE _AMreplicate #-}

_AMunfoldrN :: (Storable a) => Int -> (b -> Maybe (a, b)) -> b -> IO (CMArray a)
_AMunfoldrN n f x0 = do
    ptr <- mallocForeignPtrArray n
    withForeignPtr ptr $ \arr ->
        fix `flip` 0 `flip` x0 $ \loop !i !x ->
            case f x of
                Just (y, x') -> do
                    pokeElemOff arr i y
                    loop (i + 1) x'
                Nothing -> return $ CMA 0 n ptr
{-# INLINE _AMunfoldrN #-}

_AMunsafeRead :: (Storable a) => CMArray a -> Int -> IO a
_AMunsafeRead (CMA _ l ptr) i = assert (0 <= i && i < l)
    . withForeignPtr ptr $ \arr ->
        peekElemOff arr i
{-# INLINE _AMunsafeRead #-}

_AMunsafeWrite :: (Storable a) => CMArray a -> Int -> a -> IO ()
_AMunsafeWrite (CMA _ l ptr) i x = assert (0 <= i && i < l)
    . withForeignPtr ptr $ \arr ->
        pokeElemOff arr i x
{-# INLINE _AMunsafeWrite #-}

_AMunsafeModify :: (Storable a) => CMArray a -> Int -> (a -> a) -> IO ()
_AMunsafeModify (CMA _ l ptr) i f = assert (0 <= i && i < l)
    . withForeignPtr ptr $ \arr ->
        peekElemOff arr i >>= pokeElemOff arr i . f
{-# INLINE _AMunsafeModify #-}

_AMreadArray :: (Storable a) => CMArray a -> Int -> IO a
_AMreadArray (CMA o l ptr) i = assert (0 <= i && i < l)
    . withForeignPtr ptr $ \arr ->
        peekElemOff arr (i + o)
{-# INLINE _AMreadArray #-}

_AMwriteArray :: (Storable a) => CMArray a -> Int -> a -> IO ()
_AMwriteArray (CMA o l ptr) i x = assert (0 <= i && i < l)
    . withForeignPtr ptr $ \arr ->
        pokeElemOff arr (i + o) x
{-# INLINE _AMwriteArray #-}

_AMmodifyArray :: (Storable a) => CMArray a -> Int -> (a -> a) -> IO ()
_AMmodifyArray (CMA o l ptr) i f = assert (0 <= i && i < l)
    . withForeignPtr ptr $ \arr ->
        peekElemOff arr (i + o) >>= pokeElemOff arr (i + o) . f
{-# INLINE _AMmodifyArray #-}

_AMset :: (Storable a) => CMArray a -> a -> IO ()
_AMset (CMA o l ptr) x = withForeignPtr ptr $ \arr -> do
    let end = o + l
    flip fix o $ \loop !i ->
        when (i < end) $ do
            pokeElemOff arr i x
            loop (i + 1)
{-# INLINE _AMset #-}

_AMfoldl' :: (Storable b) => (a -> b -> a) -> a -> CMArray b -> IO a
_AMfoldl' f x0 arr@(CMA o l _) = foldr step return [o..o+l-1] x0
  where
    step i k = \ !acc -> do
        ai <- _AMunsafeRead arr i
        k $ f acc ai
{-# INLINE _AMfoldl' #-}

_AMfoldr' :: (Storable b) => (b -> a -> a) -> a -> CMArray b -> IO a
_AMfoldr' f x0 arr@(CMA o l _) = foldl step return [o..o+l-1] x0
  where
    step k i = \ !acc -> do
        ai <- _AMunsafeRead arr i
        k $ f ai acc
{-# INLINE _AMfoldr' #-}

_AMfoldl'M :: (Storable b) => (a -> b -> IO a) -> a -> CMArray b -> IO a
_AMfoldl'M f x0 arr@(CMA o l _) = foldr step return [o..o+l-1] x0
  where
    step i k = \ !acc -> do
        ai <- _AMunsafeRead arr i
        f acc ai >>= k
{-# INLINE _AMfoldl'M #-}

_AMtraverse_ :: (Storable a) => (a -> IO ()) -> CMArray a -> IO ()
_AMtraverse_ f arr@(CMA o l _) = foldr step (return()) [o..o+l-1]
  where
    step i k = do
        ai  <- _AMunsafeRead arr i
        f ai >> k
{-# INLINE _AMtraverse_ #-}

_AMfor_ :: (Storable a) => CMArray a -> (a -> IO ()) -> IO ()
_AMfor_ = flip _AMtraverse_
{-# INLINE _AMfor_ #-}

_AMrtraverse_ :: (Storable a) => (a -> IO ()) -> CMArray a -> IO ()
_AMrtraverse_ f arr@(CMA o l _) = foldr (step.negate) (return()) [1-o-l.. -o]
  where
    step i k = do
        ai  <- _AMunsafeRead arr i
        f ai >> k
{-# INLINE _AMrtraverse_ #-}

_AMrfor_ :: (Storable a) => CMArray a -> (a -> IO ()) -> IO ()
_AMrfor_ = flip _AMrtraverse_
{-# INLINE _AMrfor_ #-}

_AMitraverse_ :: (Storable a) => (Int -> a -> IO ()) -> CMArray a -> IO ()
_AMitraverse_ f arr@(CMA o l _) = foldr (step.negate) (return()) [1-o-l.. -o]
  where
    step i k = do
        ai  <- _AMunsafeRead arr i
        f i ai >> k
{-# INLINE _AMitraverse_ #-}

_AMifor_ :: (Storable a) => CMArray a -> (Int -> a -> IO ()) -> IO ()
_AMifor_ = flip _AMitraverse_
{-# INLINE _AMifor_ #-}

instance (Storable a) => Storable (a, a) where
    sizeOf _ = 2 * sizeOf (undefined :: a)
    {-# INLINE sizeOf #-}
    alignment _ = alignment (undefined :: a)
    {-# INLINE alignment #-}
    peek p  = do
        let pa = castPtr p
        !a <- peek pa
        !b <- peekElemOff pa 1
        return (a, b)
    {-# INLINE peek #-}
    poke p (a, b) = do
        let pa = castPtr p
        poke pa a
        pokeElemOff pa 1 b
    {-# INLINE poke #-}
