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
import           System.Environment
import           System.IO
import           System.Process
import           Unsafe.Coerce

#define MOD 1000000007

main :: IO ()
main = withInteractive . lift $ do
    n <- recvLn
    dist <- mapM (\i -> flip (,) i <$> query 1 i) [2..n]
    let v = snd $ maximum dist
    dist' <- mapM (query v) $ [1..v-1] ++ [v+1..n]
    sendStrLn $ unwords ["!", show (maximum dist')]

query :: (MonadInteractive m) => Int -> Int -> m Int
query a b = do
    sendStrLn $ unwords ["?", show a, show b]
    recvLn

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
-- Control.Monad.Interactive
-------------------------------------------------------------------------------
class (Monad m) => MonadInteractive m where { {-# MINIMAL sendStr , recvLine #-}; sendStr :: String -> m (); recvLine :: m String; send :: (Show a) => a -> m (); send = sendStrLn . show; sendStrLn :: String -> m (); sendStrLn cs = sendStr cs >> sendStr "\n"; recvLn :: (Read a) => m a; recvLn = read <$> recvLine}
data InteractiveHandle = InteractiveHandle{hin :: Handle, hout :: Handle, mherr :: Maybe Handle}
createInteractiveHandle :: IO InteractiveHandle
createInteractiveHandle = getArgs >>= \case { [] -> return $ InteractiveHandle stdin stdout Nothing; args -> do { let { cmds = filter (not . L.isPrefixOf "-") args}; (Just hout, Just hin, _, _) <- createProcess (shell . unwords $ "python3 local_testing_tool.py" : cmds){std_in = CreatePipe, std_out = CreatePipe}; let { mherr | elem "--verbose" args = Just stderr | otherwise = Nothing}; return $ InteractiveHandle{..}}}
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
