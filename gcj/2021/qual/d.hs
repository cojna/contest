{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, CPP, DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, LambdaCase    #-}
{-# LANGUAGE MagicHash, MultiParamTypeClasses, MultiWayIf              #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, RankNTypes            #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, StandaloneDeriving  #-}
{-# LANGUAGE TupleSections, TypeApplications, TypeFamilies, TypeInType #-}
{-# LANGUAGE UnboxedTuples, ViewPatterns                               #-}

module Main where
import           Control.Applicative
import           Control.Exception
import           Control.Monad                     hiding (fail)
import           Control.Monad.Fail
import           Control.Monad.Primitive
import           Control.Monad.Reader              hiding (fail)
import           Control.Monad.ST
import           Control.Monad.State.Strict        hiding (fail)
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
import           Data.Functor
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
import           Prelude                           hiding (fail)
import           System.Environment
import           System.IO
import           System.Process
import           Unsafe.Coerce

#define MOD 1000000007 /* 998244353 */

main :: IO ()
main = withGCJInteractive $ \[n, q] -> do
    solve n q >>= answer

solve :: Int -> Int -> Interactive [Int]
solve n q = do
    state0 <- query 1 2 3 >>= \case
        1 -> pure [3,1,2]
        2 -> pure [1,2,3]
        3 -> pure [2,3,1]
    foldM step state0 [4..n]

step :: [Int] -> Int -> Interactive [Int]
step xs0 i = go 0 n
    where
        !n = i - 1
        !xs = U.fromListN n xs0
        go !l !r
            | l == r = pure $ take l xs0 ++ i : drop l xs0
            | r - l == 1 = if l == 0 then go 0 2 else go (l - 1) r
            | r - l == 2 = do
                res <- query  i (xs U.! l) (xs U.! (l + 1))
                if res == i
                then go (l + 1) (l + 1)
                else if res == xs U.! l
                then go l l
                else go r r
            | otherwise = do
                res <- query i (xs U.! m1) (xs U.! m2)
                if res == i
                then go (m1 + 1) m2
                else if res == xs U.! m1
                then go l m1
                else go (m2 + 1) r
            where
                m1 = l + div (r - l) 3
                m2 = l + 2 * div (r - l) 3

query :: Int -> Int -> Int -> Interactive Int
query x y z = interactive $ \_ wa k -> do
    sendStrLn . unwords $ map show [x, y, z]
    res <- recvLn
    case res of
        (-1) -> wa
        med  -> k med

answer :: [Int] -> Interactive ()
answer xs = interactive $ \ac wa _ -> do
    sendStrLn . unwords $ map show xs
    res <- recvLn
    case res of
        1    -> ac
        (-1) -> wa
        _    -> fail "unreachable"


withGCJInteractive :: ([Int] -> Interactive a) -> IO ()
withGCJInteractive f = withInteractive $ do
  (t : params) <- map read . words <$> lift recvLine
  foldr ((<|>) . const (f params)) empty [1 .. t]


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
-------------------------------------------------------------------------------
-- Control.GCJ
-------------------------------------------------------------------------------
formatGCJ :: Int -> String
formatGCJ i = "Case #" <> shows i ": "
withGCJ :: IO () -> IO ()
withGCJ f = getArgs >>= \case { ["--debug"] -> f; [] -> do { t <- readLn; mapM_ ((*> f) . putStr . formatGCJ) [1 .. t]}; args -> error $ show args}

-------------------------------------------------------------------------------
-- Control.Monad.Interactive
-------------------------------------------------------------------------------
class (Monad m) => MonadInteractive m where { {-# MINIMAL sendStr , recvLine #-}; sendStr :: String -> m (); recvLine :: m String; send :: (Show a) => a -> m (); send = sendStrLn . show; sendStrLn :: String -> m (); sendStrLn cs = sendStr cs >> sendStr "\n"; recvLn :: (Read a) => m a; recvLn = read <$> recvLine}
data InteractiveHandle = InteractiveHandle{hin :: Handle, hout :: Handle, mherr :: Maybe Handle}
createInteractiveHandle :: IO InteractiveHandle
createInteractiveHandle = getArgs >>= \case { [] -> return $ InteractiveHandle stdin stdout Nothing; args -> do { let { cmds = filter (not . L.isPrefixOf "-") args}; (Just hout, Just hin, _, _) <- createProcess (shell . unwords $ "python3 local_testing_tool.py" : cmds){std_in = CreatePipe, std_out = CreatePipe}; let { mherr | "--verbose" `elem` args = Just stderr | otherwise = Nothing}; return $ InteractiveHandle{..}}}
withInteractiveHandle :: ReaderT InteractiveHandle IO a -> IO a
withInteractiveHandle f = createInteractiveHandle >>= runReaderT f
sendDebugFormat :: String -> String
sendDebugFormat = ("> " ++)
recvDebugFormat :: String -> String
recvDebugFormat = ("< " ++)
instance (MonadIO m) => MonadInteractive (ReaderT InteractiveHandle m) where { sendStr cs = ReaderT $ \ InteractiveHandle{..} -> liftIO $ do { mapM_ (\ herr -> hPutStr herr $ sendDebugFormat cs) mherr; hPutStr hout cs; hFlush hout}; sendStrLn cs = ReaderT $ \ InteractiveHandle{..} -> liftIO $ do { mapM_ (\ herr -> hPutStrLn herr $ sendDebugFormat cs) mherr; hPutStrLn hout cs; hFlush hout}; recvLine = ReaderT $ \ InteractiveHandle{..} -> liftIO $ do { res <- hGetLine hin; mapM_ (\ herr -> hPutStrLn herr $ recvDebugFormat res) mherr; return res}}
type Result m r = m r
type Accepted m r = Result m r
type Failed m r = Result m r
type Running a m r = a -> Result m r
type JudgeInternal m a r = Accepted m r -> Failed m r -> Running a m r -> Result m r
newtype Judge m a = Judge{unJudge :: forall r . JudgeInternal m a r}
instance Functor (Judge m) where { fmap f m = Judge $ \ ac wa k -> unJudge m ac wa (k . f); {-# INLINE fmap #-}}
instance Applicative (Judge m) where { pure x = Judge $ \ _ _ k -> k x; {-# INLINE pure #-}; mf <*> mx = Judge $ \ ac wa k -> unJudge mf ac wa $ \ f -> unJudge mx ac wa (k . f); {-# INLINE (<*>) #-}}
instance Alternative (Judge m) where { empty = Judge $ \ ac _ _ -> ac; {-# INLINE empty #-}; mx <|> my = Judge $ \ ac wa k -> unJudge mx (unJudge my ac wa k) wa k; {-# INLINE (<|>) #-}}
instance Monad (Judge m) where { mx >>= f = Judge $ \ ac wa k -> unJudge mx ac wa $ \ x -> unJudge (f x) ac wa k; {-# INLINE (>>=) #-}}
instance MonadFail (Judge m) where { fail _ = Judge $ \ _ wa _ -> wa; {-# INLINE fail #-}}
instance MonadTrans Judge where { lift m = Judge $ \ _ _ k -> m >>= k; {-# INLINE lift #-}}
instance PrimMonad m => PrimMonad (Judge m) where { type PrimState (Judge m) = PrimState m; primitive = lift . primitive; {-# INLINE primitive #-}}
type Interactive a = Judge (ReaderT InteractiveHandle IO) a
interactive :: (forall r . JudgeInternal (ReaderT InteractiveHandle IO) a r) -> Interactive a
interactive = Judge
runInteractive_ :: Interactive a -> ReaderT InteractiveHandle IO ()
runInteractive_ m = unJudge m (return ()) (return ()) (const $ return ())
withInteractive :: Interactive a -> IO ()
withInteractive = withInteractiveHandle . runInteractive_
