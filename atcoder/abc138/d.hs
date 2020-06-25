{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
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
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import qualified Data.List                   as L
import qualified Data.Map.Strict             as M
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.MutVar
import qualified Data.Set                    as S
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Debug.Trace
import           Foreign                     hiding (void)
import           GHC.Exts
import qualified System.IO                   as IO
import           Unsafe.Coerce

main :: IO ()
main = do
    [n, q] <- map read.words <$> getLine :: IO [Int]
    (tree, query) <- U.splitAt(n-1).U.unfoldr parseInt2 <$> C.getContents
    putStrLn.unwords.map show.U.toList $ solve n q
        (U.map(\(x,y)->(x-1,y-1))tree)
        (U.map(\(x,y)->(x-1,y)) query)

solve :: Int -> Int -> U.Vector Edge -> U.Vector (Int, Int) -> U.Vector Int
solve n q edges queries = U.generate n ((processed U.!).(left U.!))
  where
    tree = fromEdges n edges
    EulerTour vertices !left = eulerTour tree
    !right = U.accumulate (flip const) (U.replicate n nothing)
        $ U.imap (flip (,)) vertices
    query (root, x) = U.fromList [(left U.! root, x), (right U.! root + 1, -x)]
    !processed = U.init
        . U.postscanl' (+) 0
        . U.accumulate (+) (U.replicate (2 * n) 0)
        $ U.concatMap query queries

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = V.Vector (U.Vector Vertex)

nothing :: Int
nothing = -1

fromEdges :: Int -> U.Vector Edge -> Graph
fromEdges numV edges = V.map (U.force . U.fromList)
    . V.unsafeAccumulate (flip (:)) (V.replicate numV [])
    $ U.convert edges


data EulerTour = EulerTour (U.Vector Int) (U.Vector Int)
    deriving (Eq, Show)

subtree :: Vertex -> EulerTour -> (Int, Int)
subtree root (EulerTour l r) = (U.unsafeIndex l root, U.unsafeIndex r root)

eulerTour :: Vertex -> Graph -> EulerTour
eulerTour root tree =  runST $ do
    left <- UM.replicate (V.length tree) nothing
    right <- UM.replicate (V.length tree) nothing

    fix `flip` root `flip` nothing `flip` 0 $ \dfs v p i -> do
        UM.unsafeWrite left v i
        let nexts = U.filter (/= parent) $ V.unsafeIndex tree v
        U.foldM' `flip` (i + 1) `flip` nexts $ \i' u -> do
            j <- dfs u v i'
            UM.unsafeWrite right v j
            return (j + 1)
{-

    void $ fix `flip` root `flip` nothing `flip` 0 $ \dfs v p i -> do
        UM.unsafeWrite left v i
        U.foldM' `flip` (i + 1) `flip` V.unsafeIndex tree v $ \j u ->
            if u == p
            then return j
            else do
                k <- dfs u v j
                UM.unsafeWrite right v k
                return $ k + 1
-}
    EulerTour
        <$> U.unsafeFreeze left
        <*> U.unsafeFreeze right

-------------------------------------------------------------------------------
type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)

parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)