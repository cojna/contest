{-# LANGUAGE BangPatterns, CPP #-}

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Function
import qualified Data.List                   as L
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified System.IO                   as IO
import           System.Process


main :: IO ()
main = runInteractive $ \hin hout b -> do
    buf <- UM.replicate b (-1)
    let other i = b - 1 - i

    flip fix (0, Nothing, Nothing) $ \loop (!pos, !comp, !rev) -> do
        case (comp, rev) of
            (Just (posC, prevC), Just (posR, prevR)) -> do
                [curC, curR] <- query hin hout [posC, posR]
                let queryL = map (`mod` b) [pos..pos+3]
                let queryR = map other queryL
                ls <- query hin hout queryL
                rs <- query hin hout queryR
                when (prevC /= curC) $ do
                    flipBuf buf
                when (prevR /= curR && prevC == curC || prevR == curR && prevC /= curC) $ do
                    revBuf buf
                writeBuf buf queryL ls
                writeBuf buf queryR rs

                when (pos + 3 < div b 2) $ do
                    loop (pos + 4, Just (posC, curC), Just (posR, curR))
            (Just (posC, prevC), Nothing) -> do
                [curC, _] <- query hin hout [posC, posC]
                let queryL = map (`mod` b) [pos..pos+3]
                let queryR = map other queryL
                ls <- query hin hout queryL
                rs <- query hin hout queryR
                when (prevC /= curC) $ do
                    flipBuf buf
                writeBuf buf queryL ls
                writeBuf buf queryR rs
                let rev' = fmap (\(i, l, _) -> (i, l))
                        . L.find (\(_, l, r) -> l /= r) $ zip3 queryL ls rs
                when (pos + 3 < div b 2) $ do
                    loop (pos + 4, Just (posC, curC), rev')
            (Nothing, Just (posR, prevR)) -> do
                [curR, _] <- query hin hout [posR, posR]
                let queryL = map (`mod` b) [pos..pos+3]
                let queryR = map other queryL
                ls <- query hin hout queryL
                rs <- query hin hout queryR
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
                ls <- query hin hout queryL
                rs <- query hin hout queryR
                writeBuf buf queryL ls
                writeBuf buf queryR rs
                let comp' = fmap (\(i, l, _) -> (i, l))
                        . L.find (\(_, l, r) -> l == r) $ zip3 queryL ls rs
                let rev' = fmap (\(i, l, _) -> (i, l))
                        . L.find (\(_, l, r) -> l /= r) $ zip3 queryL ls rs
                loop (pos + 5, comp', rev')
    hDumpBuf hout buf
    IO.hFlush hout
    "Y" <- IO.hGetLine hin
    return ()

writeBuf :: (PrimMonad m) => UM.MVector (PrimState m) Int -> [Int] -> [Int] -> m ()
writeBuf mv ks vs = do
    forM_ (zip ks vs) $ \(k, v) -> do
        UM.unsafeWrite mv k v


hDumpBuf :: IO.Handle -> UM.MVector (PrimState IO) Int -> IO ()
hDumpBuf h mv = do
    v <- U.freeze mv
    IO.hPutStrLn h . concatMap show $ U.toList v


revBuf :: (PrimMonad m) => UM.MVector (PrimState m) Int -> m ()
revBuf mv = do
    let n = UM.length mv
    rep (quot n 2) $ \i -> do
        UM.unsafeSwap mv i (n - i - 1)

flipBuf :: (PrimMonad m) => UM.MVector (PrimState m) Int -> m ()
flipBuf mv = do
    rep (UM.length mv) $ \i -> do
        UM.unsafeModify mv (1-) i


query :: IO.Handle -> IO.Handle -> [Int] -> IO [Int]
query hin hout poss = do
    mapM_ (IO.hPrint hout . succ) poss
    IO.hFlush hout
    res <- mapM (const $ read <$> IO.hGetLine hin) poss
    return res


rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}


runInteractive :: (IO.Handle -> IO.Handle -> Int -> IO ()) -> IO ()
runInteractive action = do
#ifdef DEBUG
    (Just hout, Just hin, _, _) <- createProcess
        (shell "python3 local_testing_tool.py 2")
            { std_in = CreatePipe, std_out = CreatePipe}
#else
    let hin = IO.stdin
    let hout = IO.stdout
#endif
    [t, b] <- map read.words <$> IO.hGetLine hin :: IO [Int]
    forM_ [1..t] $ \i -> do
        IO.hPutStrLn IO.stderr $ "Case #" ++ shows i ":"
        action hin hout b
