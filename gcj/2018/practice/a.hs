import           Control.Monad
import           Data.Function
import           Debug.Trace
import           System.IO

main :: IO ()
main = runGCJ $ do
    [a, b] <- map read.words <$> getLine :: IO [Int]
    n <- readLn :: IO Int
    run n queryHandler client (a + 1, b + 1)

type GCJRequest = Int
data GCJResponse = TOO_SMALL | CORRECT | TOO_BIG | WRONG_ANSWER deriving (Read, Show)

type ClientState = (Int, Int)

queryHandler :: QueryHandler
queryHandler x = do
    print x
    hFlush stdout
    readLn

client :: Client
client (a, b) = (req, handler)
  where
    req = div (a + b) 2
    handler res = case res of
        CORRECT      -> Nothing
        WRONG_ANSWER -> Nothing
        TOO_BIG      -> Just (a, req - 1)
        TOO_SMALL    -> Just (req + 1, b)

runGCJ :: IO () -> IO ()
runGCJ main_ = do
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
createServer handler s req = do
    res <- handler req
    traceM $ unlines
         [ "Query " ++ shows (queryCount s + 1) "/" ++ shows(queryLimit s) ":"
         , "> " ++ show req
         , "< " ++ show res
         ]
    return (succCount s, res)

run :: Int -> QueryHandler -> Client -> ClientState -> IO ()
run limit queryHandler client = go (initialServerState limit)
  where
    server = createServer queryHandler
    go s c =  do
        let (req, handler) = client c
        (s', res) <- server s req
        case handler res of
            Just c' -> go s' c'
            Nothing -> return ()
