{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Bits
import           Data.Function
import qualified Data.Set      as S
import           Data.Word
import           Debug.Trace
import           System.Exit
import           System.IO

main :: IO ()
main = runGCJ $ do
    a <- readLn
    let limit = 1000
    run limit queryHandler client $ clientState a

client :: ClientState -> (GCJRequest, GCJResponse -> Maybe ClientState)
client (CS w h rest) = (request, handler)
  where
    request = case S.findMin rest of
        (1, _) -> (2, 2)
        (x, _) -> (min (w - 1) x, 2)
    handler res = case res of
        CORRECT      -> Nothing
        WRONG_ANSWER -> Nothing
        Digged x y   -> Just $ CS w h (S.delete (x, y) rest)

data ClientState = CS !Int !Int !(S.Set(Int, Int))

clientState :: Int -> ClientState
clientState a = CS width height $ S.fromList $ (,) <$> [1..width] <*> [1..height]
  where
    width  = 67
    height = 3

type GCJRequest = (Int, Int)
data GCJResponse
    = Digged !Int !Int
    | CORRECT
    | WRONG_ANSWER
    deriving Show

queryHandler :: GCJRequest -> IO GCJResponse
queryHandler (x, y) = do
    putStrLn . unwords $ map show [x, y]
    hFlush stdout
    response <- map read.words <$> getLine
    case response of
        [0, 0] -> return CORRECT
        [-1, -1] -> do
            exitSuccess
            return WRONG_ANSWER
        [rx, ry] -> return $ Digged rx ry
        _ -> exitFailure

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    hSetBuffering stdout NoBuffering
    t <- readLn
    forM_ [1..t] $ \i -> do
        main_

data ServerState = ServerState
    { queryCount :: !Int
    , queryLimit :: !Int
    } deriving Show

initialServerState :: Int -> ServerState
initialServerState limit = ServerState 0 limit

succCount :: ServerState -> ServerState
succCount s = s{queryCount = succ $ queryCount s}

type QueryHandler = GCJRequest -> IO GCJResponse
type Client = ClientState -> (GCJRequest, GCJResponse -> Maybe ClientState)
type Server = ServerState -> GCJRequest -> IO (ServerState, GCJResponse)

createServer :: QueryHandler -> Server
-- createServer _ s req | trace ("#" ++ show (queryCount s)) False = undefined
-- createServer _ s req | trace ("#" ++ shows (queryCount s) "\n" ++ "> " ++ shows req "") False = undefined
createServer handler s req = do
    res <- handler req
    return (succCount s, res)

run :: Int -> QueryHandler -> Client -> ClientState -> IO ()
run limit queryHandler client = go (initialServerState limit)
  where
    server = createServer queryHandler
    go s c =  do
        let (req, handler) = client c
        (s', res) <- server s req
        case handler res of
--            _       | trace ("< " ++ shows res "") False -> undefined
            Just c' -> go s' c'
            Nothing -> return ()
