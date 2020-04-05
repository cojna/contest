{-# LANGUAGE BangPatterns, CPP #-}

import           Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.List          as L

main :: IO ()
main = runGCJ $ do
    n <- readLn
    tasks <- forM [1..n] $ \i -> do
        [!s, !e] <- map read.words <$> getLine
        return $! Task s e i
    putStrLn . maybe "IMPOSSIBLE" id $ solve n tasks

type Time = Int
data Task = Task { start :: !Time, end :: !Time, taskId :: !Int}
  deriving (Eq, Ord, Show)

isCompleted :: Time -> Task -> Bool
isCompleted t (Task _ e _) = e <= t

data Assign = Assign {camereon :: !(Maybe Task), jamie :: !(Maybe Task)}
  deriving (Show)

complete :: Time -> Assign -> Assign
complete t (Assign c j) = Assign (c >>= f) (j >>= f)
  where
    f task
        | isCompleted t task = Nothing
        | otherwise = Just task

assign :: Task -> Assign -> Maybe (Char, Assign)
assign task (Assign Nothing j) = Just ('C', Assign (Just task) j)
assign task (Assign c Nothing) = Just ('J', Assign c (Just task))
assign task _ = Nothing

solve :: Int -> [Task] -> Maybe String
solve n tasks = go IM.empty (Assign Nothing Nothing) $ L.sort tasks
  where
    go res cur (task:ts) = do
        (assigned, next) <- assign task $ complete (start task) cur
        go (IM.insert (taskId task) assigned res) next ts
    go res _ [] = Just . map snd $ IM.toAscList res


runGCJ :: IO () -> IO ()
#ifdef DEBUG
runGCJ = id
#else
runGCJ action = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        action
#endif
