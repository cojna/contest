{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State.Strict
import           Data.Array.Base
import           Data.Array.ST                    (STUArray, runSTUArray)
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString.Char8            as B
import           Data.Char
import           Data.Coerce
import qualified Data.Foldable                    as F
import           Data.Function
import           Data.Int
import qualified Data.IntMap.Strict               as IM
import qualified Data.IntSet                      as IS
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Monoid                      hiding (First)
import           Data.Ord
import qualified Data.Set                         as S
import           Data.STRef
import           Data.Tuple
import           Data.Word
import           Debug.Trace
import           GHC.Arr                          (Array, Ix (..), STArray)
import           GHC.Exts
import           System.Exit
import           System.IO

main :: IO ()
main = do
    [h, w] <- map read.words <$> getLine
    run (createServer query) client
      (serverState $ h * w) (clientState h w)

data ClientState = CS
    { width    :: !Int
    , height   :: !Int
    , goal     :: (Int, Int)
    , used     :: S.Set (Int, Int)
    , prohibit :: S.Set (Int, Int)
    , canMove  :: S.Set(Int, Int)
    } deriving Show

clientState :: Int -> Int -> ClientState
clientState h w = CS
    { width = w
    , height = h
    , goal = (h, w)
    , used = S.singleton (1, 1)
    , prohibit = S.fromList $ filter inGrid [(h-1, w), (h, w-1)]
    , canMove = S.fromList $ filter inGrid [(1, 2),(2, 1)]
    }
  where
    inGrid (x, y) = 1 <= x && x <= h && 1 <= y && y <= w

neighbors :: ClientState -> Int -> Int -> [(Int, Int)]
neighbors cst@CS{..} x y = filter inGrid [(x+1,y),(x-1,y),(x,y+1),(x, y-1)]
  where
    inGrid (x, y) = 1 <= x && x <= height && 1 <= y && y <= width

needCell :: Int -> Int -> Int
needCell 1 w = w - 3
needCell h 1 = h - 3
needCell h w = h * w - 4

type Client = ClientState -> (GCJRequest, GCJResponse -> Maybe ClientState)
request0 :: ClientState -> GCJRequest
request0 cst@CS{..}
  | even $ needCell height width = Second
  | (x,y) <- S.findMin canMove = First x y

request :: ClientState -> GCJRequest
request cst@CS{..}
  | S.member goal canMove = Query height width
  | (x, y) <- S.findMin canMove = Query x y

handler :: ClientState -> GCJRequest -> GCJResponse -> Maybe ClientState
handler cst@CS{..} Second (P x y) = Just $ cst{ used = used', canMove = canMove' }
  where
    !used' = S.insert (x, y) used
    !canMove' = F.foldl' (flip S.insert) (S.delete (x, y) canMove)
        . filter (`S.notMember` used')
        . filter (`S.notMember` prohibit)
        $ neighbors cst x y
handler cst@CS{..} (First x0 y0) (P x y) =
    handler cst (Query x0 y0) (P x y)
handler cst@CS{..} (Query x0 y0) (P x y) = Just $ cst{ used = used', canMove = canMove' }
  where
    !used' = S.insert (x0, y0) $ S.insert (x, y) used
    !canMove' = F.foldl' (flip S.insert) (S.delete (x0, y0) $ S.delete (x, y) canMove)
        . filter (`S.notMember` used')
        . filter (`S.notMember` prohibit)
        $ neighbors cst x y ++ neighbors cst x0 y0


client :: Client
-- client cst | traceShow cst False = undefined
client cst@CS{..} | S.size used == 1, req <- request0 cst = (req, handler cst req)
client cst@CS{..} = (req, handler cst req)
  where
    req = request cst

data GCJRequest
    = Query !Int !Int
    | First !Int !Int
    | Second
    deriving Show

data GCJResponse
    = P !Int !Int
    | CORRECT
    | WRONG_ANSWER
    deriving Show

type GCJQuery = GCJRequest -> IO GCJResponse
query :: GCJQuery
query (First x y) = do
    putStrLn "First"
    query (Query x y)
query Second = do
    print Second
    hFlush stdout
    response <- map read.words <$> getLine
    case response of
        [x, y] -> return $ P x y
        _      -> exitFailure
query (Query x y) = do
    putStrLn.unwords $ map show [x, y]
    hFlush stdout
    response <- map read.words <$> getLine
    case response of
        [-1, -1] -> exitSuccess
        [x, y]   -> return $ P x y
        _        -> exitFailure


data ServerState = ServerState
    { queryCount :: !Int
    , queryLimit :: !Int
    } deriving Show

serverState :: Int -> ServerState
serverState limit = ServerState
    { queryCount = 0
    , queryLimit = limit
    }

type Server = ServerState -> GCJRequest -> IO (ServerState, GCJResponse)
createServer :: GCJQuery -> Server
createServer query s@ServerState{..} req = do
    res <- query req
    return (s{queryCount = queryCount + 1}, res)

debugServer :: GCJQuery -> Server
debugServer query s@ServerState{..} req = do
    hPutStrLn stderr $ "#" ++ show (queryCount + 1)
    hPutStrLn stderr $ "> " ++ show req
    res <- query req
    hPutStrLn stderr $ "< " ++ show res
    return (s{queryCount = queryCount + 1}, res)

run :: Server -> Client -> ServerState -> ClientState -> IO ()
run server client = fix $ \loop !s !c -> do
    let (req, handler) = client c
    (s', res) <- server s req
    case handler res of
        Just c' -> loop s' c'
        Nothing -> return ()
