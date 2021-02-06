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
import           Data.Monoid                       hiding (First (..),
                                                    Last (..))
import           Data.Ord
import           Data.Primitive
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set                          as S
import           Data.Tuple
import qualified Data.Vector                       as V
import qualified Data.Vector.Algorithms.Intro      as Intro
import qualified Data.Vector.Fusion.Bundle.Monadic as MB
import qualified Data.Vector.Fusion.Stream.Monadic as MS
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
import           System.IO
import           Unsafe.Coerce

#define MOD 1000000007 /* 998244353 */

main :: IO ()
main = runSolver $ do
    cs <- vector @U.Vector char
    return . showLnB $ solve cs

solve :: U.Vector Char -> Int
solve cs = totalDamage
    $ U.ifoldl' step (S 0 0 5 False []) (cs U.++ U.replicate 10 '-')

data S = S
    { totalDamage :: !Int
    , numCombo :: !Int
    , numKab :: !Int
    , inCharge :: !Bool
    , queries :: ![T Q]
    } deriving (Show)

data Q
    = CmdN
    | CmdC
    | NOP
    | Damage !Int
    | Return !Int
    | Uncharge
    deriving (Eq, Show)

data T a = T !Int !a
    deriving (Eq, Show)

instance (Eq a) => Ord (T a) where
    compare (T x _) (T y _) = compare x y

handleQ :: T Q -> S -> S
handleQ (T i q) s@S{..} = case q of
    CmdN | numKab > 0, not inCharge -> s
        { numKab = numKab - 1
        , queries
            = L.insert (T (i + 2) (Damage (damageN s)))
            . L.insert (T (i + 7) (Return 1))
            $ queries
        }
    CmdC | numKab >= 3, not inCharge -> s
        { numKab = numKab - 3
        , inCharge = True
        , queries
            = L.insert (T (i + 3) Uncharge)
            . L.insert (T (i + 4) (Damage (damageC s)))
            . L.insert (T (i + 9) (Return 3))
            $ queries
        }
    Damage d -> s
        { totalDamage = totalDamage + d
        , numCombo = numCombo + 1
        }
    Return x -> s
        { numKab = numKab + x
        }
    Uncharge -> s
        { inCharge = False
        }
    _ -> s

damageN :: S -> Int
damageN S{..} = 10 + quot numCombo 10

damageC :: S -> Int
damageC s = 5 * damageN s

command :: Int -> Char -> T Q
command i 'N' = T i CmdN
command i 'C' = T i CmdC
command i '-' = T i NOP

prestep :: Int -> S -> S
prestep i s@S{..}
    = F.foldl' (flip handleQ)
        s{queries = qs} qs0
    where
        (qs0,qs) = span (\(T j _) -> i == j) queries

step :: S -> Int -> Char -> S
step s i c
    = handleQ (command i c)
    $ prestep i s


-------------------------------------------------------------------------------
-- My.Prelude
-------------------------------------------------------------------------------
rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (stream 0 n)
{-# INLINE rep #-}
rep1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep1 n = flip MS.mapM_ (stream 1 (n + 1))
{-# INLINE rep1 #-}
rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev n = flip MS.mapM_ (streamR 0 n)
{-# INLINE rev #-}
rev1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev1 n = flip MS.mapM_ (streamR 1 (n + 1))
{-# INLINE rev1 #-}
stream :: (Monad m) => Int -> Int -> MS.Stream m Int
stream !l !r = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream #-}
streamR :: (Monad m) => Int -> Int -> MS.Stream m Int
streamR !l !r = MS.Stream step (r - 1) where { step x | x >= l = return $ MS.Yield x (x - 1) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] streamR #-}
stream' :: (Monad m) => Int -> Int -> Int -> MS.Stream m Int
stream' !l !r !d = MS.Stream step l where { step x | x < r = return $ MS.Yield x (x + d) | otherwise = return MS.Done; {-# INLINE [0] step #-}}
{-# INLINE [1] stream' #-}
infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL :: Int -> Int -> Int
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}
unsafeShiftRL :: Int -> Int -> Int
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}
unlinesB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unlinesB f = G.foldr' ((<>) . (<> endlB) . f) mempty
unwordsB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unwordsB f vec | G.null vec = mempty | otherwise = f (G.head vec) <> G.foldr' ((<>) . (B.char7 ' ' <>) . f) mempty (G.tail vec)
gridB :: (G.Vector v a) => Int -> Int -> (a -> B.Builder) -> v a -> B.Builder
gridB h w f mat = F.foldMap ((<> endlB) . unwordsB f) [G.slice (i * w) w mat | i <- [0 .. h - 1]]
sizedB :: (G.Vector v a) => (v a -> B.Builder) -> v a -> B.Builder
sizedB f vec = B.intDec (G.length vec) <> endlB <> f vec
yesnoB :: Bool -> B.Builder
yesnoB = bool (B.string7 "No") (B.string7 "Yes")
pairB :: (a -> B.Builder) -> (b -> B.Builder) -> (a, b) -> B.Builder
pairB f g (x, y) = f x <> B.char7 ' ' <> g y
showB :: (Show a) => a -> B.Builder
showB = B.string7 . show
showLnB :: (Show a) => a -> B.Builder
showLnB = B.string7 . flip shows "\n"
endlB :: B.Builder
endlB = B.char7 '\n'
{-# INLINE endlB #-}
runSolver :: Parser B.Builder -> IO ()
runSolver solver = do { bs <- C.getContents; case runStateT solver bs of { Just (answer, rest) -> do { unless (C.all isSpace rest) $ do { C.hPutStrLn stderr rest}; B.hPutBuilder stdout answer}; Nothing -> hPutStrLn stderr "parse error"}}
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
integer :: Parser Integer
integer = coerce $ C.readInteger . C.dropWhile isSpace
{-# INLINE integer #-}
integral :: (Integral a) => Parser a
integral = fmap fromIntegral int
{-# INLINE integral #-}
char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}
byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}
bytestring :: Parser C.ByteString
bytestring = do { skipSpaces; gets (C.findIndex isSpace) >>= \case { Just i -> state (C.splitAt i); Nothing -> state (flip (,) C.empty)}}
{-# INLINE bytestring #-}
skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}
vector :: (G.Vector v a) => Parser a -> Parser (v a)
vector p = G.unfoldr (runParser p) <$> takeLine
{-# INLINE vector #-}
vectorN :: (G.Vector v a) => Int -> Parser a -> Parser (v a)
vectorN n p = G.unfoldrN n (runParser p) <$> takeLine
{-# INLINE vectorN #-}
vectorH :: (G.Vector v a) => Int -> Parser a -> Parser (v a)
vectorH h p = G.unfoldrN h (runParser p) <$> takeLines h
{-# INLINE vectorH #-}
vectorW :: (G.Vector v a) => Int -> Parser a -> Parser (v a)
vectorW = vectorN
{-# INLINE vectorW #-}
vectorHW :: (G.Vector v a) => Int -> Int -> Parser a -> Parser (v a)
vectorHW h w p = G.unfoldrN (h * w) (runParser p) <$> takeLines h
{-# INLINE vectorHW #-}
gridHW :: Int -> Int -> Parser (U.Vector Char)
gridHW h w = U.unfoldrN (h * w) (runParser char) . C.filter (/= '\n') <$> takeLines h
{-# INLINE gridHW #-}
takeLine :: Parser C.ByteString
takeLine = state $ fmap (B.drop 1) . C.span (/= '\n')
{-# INLINE takeLine #-}
takeLines :: Int -> Parser C.ByteString
takeLines n | n > 0 = do { gets (drop (n - 1) . C.elemIndices '\n') >>= \case { (i : _) -> state (C.splitAt (i + 1)); [] -> state (flip (,) C.empty)}} | otherwise = pure C.empty
{-# INLINE takeLines #-}
neighbor4 :: (Applicative f) => Int -> Int -> Int -> (Int -> f ()) -> f ()
neighbor4 h w xy f = when (x /= 0) (f $ xy - w) *> when (y /= 0) (f $ xy - 1) *> when (y /= w - 1) (f $ xy + 1) *> when (x /= h - 1) (f $ xy + w) where { (!x, !y) = quotRem xy w}
{-# INLINE neighbor4 #-}
binarySearchM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
binarySearchM low high p = go low high where { go !low !high | high <= low = return high | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid) where { mid = low + unsafeShiftRL (high - low) 1}}
{-# INLINE binarySearchM #-}
binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch low high p = runIdentity $ binarySearchM low high (return . p)
{-# INLINE binarySearch #-}
radixSort :: U.Vector Int -> U.Vector Int
radixSort v = F.foldl' step v [0, 16, 32, 48] where { mask k x = unsafeShiftRL x k .&. 65535; step v k = U.create $ do { pos <- UM.unsafeNew 65537; UM.set pos 0; U.forM_ v $ \ x -> do { UM.unsafeModify pos (+ 1) (mask k x + 1)}; rep 65535 $ \ i -> do { fi <- UM.unsafeRead pos i; UM.unsafeModify pos (+ fi) (i + 1)}; res <- UM.unsafeNew $ U.length v; U.forM_ v $ \ x -> do { let { !masked = mask k x}; i <- UM.unsafeRead pos masked; UM.unsafeWrite pos masked $ i + 1; UM.unsafeWrite res i x}; return res}}
{-# INLINE radixSort #-}
encode32x2 :: Int -> Int -> Int
encode32x2 x y = unsafeShiftL x 32 .|. y
{-# INLINE encode32x2 #-}
decode32x2 :: Int -> (Int, Int)
decode32x2 xy = let { !x = unsafeShiftRL xy 32; !y = xy .&. 4294967295} in (x, y)
{-# INLINE decode32x2 #-}
