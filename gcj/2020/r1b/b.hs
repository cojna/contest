{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances       #-}
{-# LANGUAGE KindSignatures, LambdaCase, MagicHash, MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RankNTypes, RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeApplications         #-}
{-# LANGUAGE TypeFamilies, ViewPatterns                                   #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Reader
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
import           Data.Functor
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
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           Unsafe.Coerce

maxX, minX, maxY, minY :: Int
maxX = 1000000000
minX = -maxX
maxY = maxX
minY = minX

main :: IO ()
main = withGCJInteractive $ \[a, b] -> do
    (x0, y0) <- findInternal
    lx <- lowerBoundM minX x0 (\x -> query x y0)
    ux <- upperBoundM x0 maxX (\x -> query x y0)
    ly <- lowerBoundM minY y0 (\y -> query x0 y)
    uy <- upperBoundM y0 maxY (\y -> query x0 y)
    query (div (lx + ux) 2) (div (ly + uy) 2)

findInternal :: Interactive (Int, Int)
findInternal = do
    let qs0 = [(div dx 4, div dy 4)|dx<-[minX, maxX], dy<-[minY, maxY]]
    flip fix qs0 $ \loop ((x, y):qs) -> do
        query x y >>= \case
            True -> return (x, y)
            False -> loop qs

data Response = CENTER | HIT | MISS | WRONG
    deriving (Eq, Show, Read)

query :: Int -> Int -> Interactive Bool
query x y = interactive $ \ac wa k -> do
    sendStrLn . unwords $ map show [x, y]
    recvLn >>= \case
        CENTER -> ac
        HIT -> k True
        MISS -> k False
        WRONG -> wa

lowerBoundM :: (Integral i, Monad m) => i -> i -> (i -> m Bool) -> m i
lowerBoundM low high p = go low high
  where
    go !low !high
        | high <= low = return high
        | otherwise = do
            pmid <- p mid
            if pmid
            then go low mid
            else go (mid + 1) high
      where
        h = toInteger high
        l = toInteger low
        mid = fromIntegral $ l + div (h - l) 2

{-# INLINE lowerBoundM #-}

upperBoundM :: (Integral i, Monad m) => i -> i -> (i -> m Bool) -> m i
upperBoundM low high p = do
    phigh <- p high
    if phigh
    then return high
    else subtract 1 <$> lowerBoundM low high (fmap not.p)
{-# INLINE upperBoundM #-}

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
-- Control.GCJ
-------------------------------------------------------------------------------

formatGCJ :: Int -> String
formatGCJ i = "Case #" <> shows i ": "

withGCJ :: IO () -> IO ()
withGCJ f = getArgs >>= \case
    ["--debug"] -> f
    [] -> do
        t <- readLn
        mapM_ ((*> f) . putStrLn . formatGCJ) [1..t]
    args -> error $ show args

withGCJInteractive :: ([Int] -> Interactive a) -> IO ()
withGCJInteractive f = withInteractive $ do
    (t:params) <- map read.words <$> lift recvLine
    foldr ((<|>).const (f params)) empty [1..t]

class (Monad m) => MonadInteractive m where
    {-# MINIMAL sendStr, recvLine #-}
    sendStr :: String -> m ()
    recvLine :: m String

    send :: (Show a) => a -> m ()
    send = sendStrLn . show

    sendStrLn :: String -> m ()
    sendStrLn cs = sendStr cs >> sendStr "\n"

    recvLn :: (Read a) => m a
    recvLn = read <$> recvLine


data InteractiveHandle = InteractiveHandle
    { hin  :: Handle
    , hout :: Handle
    , mherr :: Maybe Handle
    }

createInteractiveHandle :: IO InteractiveHandle
createInteractiveHandle = getArgs >>= \case
    [] -> return $ InteractiveHandle stdin stdout Nothing
    args -> do
        let cmds = filter (not.L.isPrefixOf "-") args
        (Just hout, Just hin, _, _) <- createProcess
            (shell . unwords $ "python3 local_testing_tool.py": cmds)
                { std_in = CreatePipe
                , std_out = CreatePipe
                }
        let mherr | elem "--verbose" args = Just stderr
                  | otherwise = Nothing
        return $ InteractiveHandle{..}

withInteractiveHandle :: ReaderT InteractiveHandle IO a -> IO a
withInteractiveHandle f = createInteractiveHandle >>= runReaderT f

sendDebugFormat :: String -> String
sendDebugFormat = ("> " ++)

recvDebugFormat :: String -> String
recvDebugFormat = ("< " ++)

instance (MonadIO m) => MonadInteractive (ReaderT InteractiveHandle m) where
    sendStr cs = ReaderT $ \InteractiveHandle{..} ->
        liftIO $ do
            mapM_ (\herr -> hPutStr herr $ sendDebugFormat cs) mherr
            hPutStr hout cs
            hFlush hout


    sendStrLn cs = ReaderT $ \InteractiveHandle{..} ->
        liftIO $ do
            mapM_ (\herr -> hPutStrLn herr $ sendDebugFormat cs) mherr
            hPutStrLn hout cs
            hFlush hout
    recvLine = ReaderT $ \InteractiveHandle{..} ->
        liftIO $ do
            res <- hGetLine hin
            mapM_ (\herr -> hPutStrLn herr $ recvDebugFormat res) mherr
            return res

type Result m r = m r
type Accepted m r = Result m r
type Failed m r = Result m r
type Running a m r = a -> Result m r
type JudgeInternal m a r
    = Accepted m r -> Failed m r -> Running a m r -> Result m r

newtype Judge m a = Judge {unJudge :: forall r . JudgeInternal m a r}

instance Functor (Judge m) where
    fmap f m = Judge $ \ac wa k ->
        unJudge m ac wa (k . f)
    {-# INLINE fmap #-}

instance Applicative (Judge m) where
    pure x = Judge $ \_ _ k -> k x
    {-# INLINE pure #-}
    mf <*> mx = Judge $ \ac wa k ->
        unJudge mf ac wa $ \f ->
            unJudge mx ac wa (k . f)
    {-# INLINE (<*>) #-}

instance Alternative (Judge m) where
    empty = Judge $ \ac _ _ -> ac
    {-# INLINE empty #-}
    mx <|> my = Judge $ \ac wa k ->
        unJudge mx
            (unJudge my ac wa k) wa k
    {-# INLINE (<|>) #-}

instance Monad (Judge m) where
    mx >>= f = Judge $ \ac wa k ->
        unJudge mx ac wa $ \x ->
            unJudge (f x) ac wa k
    {-# INLINE (>>=) #-}

instance MonadTrans Judge where
    lift m = Judge $ \_ _ k -> m >>= k
    {-# INLINE lift #-}

instance PrimMonad m => PrimMonad (Judge m) where
    type PrimState (Judge m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}

type Interactive a = Judge (ReaderT InteractiveHandle IO) a

interactive
    :: (forall r . JudgeInternal (ReaderT InteractiveHandle IO) a r)
    -> Interactive a
interactive = Judge

runInteractive_ :: Interactive a -> ReaderT InteractiveHandle IO ()
runInteractive_ m = unJudge m (return ()) (return ()) (const $ return ())

withInteractive :: Interactive a -> IO ()
withInteractive = withInteractiveHandle . runInteractive_
