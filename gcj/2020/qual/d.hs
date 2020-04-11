{-# LANGUAGE BangPatterns, CPP, ImplicitParams, RankNTypes, ConstraintKinds#-}

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Function
import qualified Data.List                   as L
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           System.Environment
import qualified System.IO                   as IO
import           System.IO
import           System.Process


main :: IO ()
main = withInteractiveJudge $ \b -> do
    buf <- UM.replicate b (-1)
    let other i = b - 1 - i

    flip fix (0, Nothing, Nothing) $ \loop (!pos, !comp, !rev) -> do
        case (comp, rev) of
            (Just (posC, prevC), Just (posR, prevR)) -> do
                [curC, curR] <- query [posC, posR]
                let queryL = map (`mod` b) [pos..pos+3]
                let queryR = map other queryL
                ls <- query queryL
                rs <- query queryR
                when (prevC /= curC) $ do
                    flipBuf buf
                when (prevR /= curR && prevC == curC || prevR == curR && prevC /= curC) $ do
                    revBuf buf
                writeBuf buf queryL ls
                writeBuf buf queryR rs

                when (pos + 3 < div b 2) $ do
                    loop (pos + 4, Just (posC, curC), Just (posR, curR))
            (Just (posC, prevC), Nothing) -> do
                [curC, _] <- query [posC, posC]
                let queryL = map (`mod` b) [pos..pos+3]
                let queryR = map other queryL
                ls <- query queryL
                rs <- query  queryR
                when (prevC /= curC) $ do
                    flipBuf buf
                writeBuf buf queryL ls
                writeBuf buf queryR rs
                let rev' = fmap (\(i, l, _) -> (i, l))
                        . L.find (\(_, l, r) -> l /= r) $ zip3 queryL ls rs
                when (pos + 3 < div b 2) $ do
                    loop (pos + 4, Just (posC, curC), rev')
            (Nothing, Just (posR, prevR)) -> do
                [curR, _] <- query [posR, posR]
                let queryL = map (`mod` b) [pos..pos+3]
                let queryR = map other queryL
                ls <- query  queryL
                rs <- query  queryR
                when (prevR /= curR) $ do
                    revBuf buf
                writeBuf buf queryL ls
                writeBuf buf queryR rs
                let comp' = fmap (\(i, l, _) -> (i, l))
                        . L.find (\(_, l, r) -> l == r) $ zip3 queryL ls rs
                when (pos + 3 < div b 2) $ do
                    loop (pos + 4, comp', Just (posR, curR))
            (Nothing, Nothing) -> do
                let queryL = [pos..pos+4]
                let queryR = map other queryL
                ls <- query  queryL
                rs <- query  queryR
                writeBuf buf queryL ls
                writeBuf buf queryR rs
                let comp' = fmap (\(i, l, _) -> (i, l))
                        . L.find (\(_, l, r) -> l == r) $ zip3 queryL ls rs
                let rev' = fmap (\(i, l, _) -> (i, l))
                        . L.find (\(_, l, r) -> l /= r) $ zip3 queryL ls rs
                loop (pos + 5, comp', rev')
    hDumpBuf buf
    "Y" <- recvLine
    return ()

writeBuf :: (PrimMonad m) => UM.MVector (PrimState m) Int -> [Int] -> [Int] -> m ()
writeBuf mv ks vs = do
    forM_ (zip ks vs) $ \(k, v) -> do
        UM.unsafeWrite mv k v


hDumpBuf :: HasInteractiveJudge => UM.MVector (PrimState IO) Int -> IO ()
hDumpBuf mv = do
    v <- U.freeze mv
    sendStrLn . concatMap show $ U.toList v


revBuf :: (PrimMonad m) => UM.MVector (PrimState m) Int -> m ()
revBuf mv = do
    let n = UM.length mv
    rep (quot n 2) $ \i -> do
        UM.unsafeSwap mv i (n - i - 1)

flipBuf :: (PrimMonad m) => UM.MVector (PrimState m) Int -> m ()
flipBuf mv = do
    rep (UM.length mv) $ \i -> do
        UM.unsafeModify mv (1-) i


query :: HasInteractiveJudge => [Int] -> IO [Int]
query poss = do
    mapM_ (send . succ) poss
    res <- mapM (const $ recvLn) poss
    return res


rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}

withInteractiveJudge :: (HasInteractiveJudge => Int -> IO ()) -> IO ()
withInteractiveJudge action = do
    judge <- newInteractiveJudge
    let ?interactiveJudge = judge
    [t, b] <- map read.words <$> recvLine :: IO [Int]
    forM_ [1..t] $ \i -> do
        hPutStrLn stderr $ "Case #" ++ shows i ":"
        action b

data InteractiveJudge = InteractiveJudge
    { hin :: Handle
    , hout :: Handle
    }

type HasInteractiveJudge = (?interactiveJudge :: InteractiveJudge)

newInteractiveJudge :: IO InteractiveJudge
newInteractiveJudge = do
    args <- getArgs
    case args of
        [] -> return $ InteractiveJudge stdin stdout
        _ -> do
            (Just i, Just o, _, _) <- createProcess
                (shell . unwords $ "python3 local_testing_tool.py": args)
                    { std_in = CreatePipe, std_out = CreatePipe}
            return $ InteractiveJudge o i

send :: (Show a, HasInteractiveJudge) => a -> IO ()
send = sendStrLn . show

sendStr :: HasInteractiveJudge => String -> IO ()
sendStr cs = do
    IO.hPutStr (hout ?interactiveJudge) cs
    IO.hFlush (hout ?interactiveJudge)

sendStrLn :: HasInteractiveJudge => String -> IO ()
sendStrLn cs = do
    IO.hPutStrLn (hout ?interactiveJudge) cs
    IO.hFlush (hout ?interactiveJudge)

recvLine :: HasInteractiveJudge => IO String
recvLine = IO.hGetLine (hin ?interactiveJudge)

recvLn :: (Read a, HasInteractiveJudge) => IO a
recvLn = read <$> recvLine