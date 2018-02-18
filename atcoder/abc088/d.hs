{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as IS
import           Data.List
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
import           Unsafe.Coerce

main :: IO ()
main = do
    [h, w] <- map read.words <$> getLine :: IO [Int]
    m <- B.concat <$> replicateM h B.getLine
    print $ solve h w m

inf :: Int
inf = 0x3f3f3f3f

solve :: Int -> Int -> B.ByteString -> Int
solve h w bs
    | dist == inf = -1
    | otherwise = h * w - numBlock - dist
  where
    numBlock = B.length $ B.filter (== '#') bs
    isOK (x, y) = B.unsafeIndex bs (x * w + y) == 46 -- chr 46 == '.'
    idx ::Int -> Int -> Int
    idx x y = x * w + y
    {-# INLINE idx #-}
    inGrid (x, y) = 0 <= x && x < h && 0 <= y && y < w
    neighbors x y = filter (\xy -> isOK xy && inGrid xy) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    dist = runST $ do
        d <- UM.replicate (h*w) inf
        let bfs ((x,y):fs) rs = do
                dxy <- UM.unsafeRead d (idx x y)
                nexts <- flip filterM (neighbors x y) $ \(nx,ny) -> do
                    dnxny <- UM.unsafeRead d (idx nx ny)
                    if dxy + 1 < dnxny
                    then do
                        UM.unsafeWrite d (idx nx ny) $ dxy + 1
                        return True
                    else return False
                bfs fs (nexts ++ rs)
            bfs [] [] = return ()
            bfs [] rs = bfs (reverse rs) []
        UM.unsafeWrite d (idx 0 0) 1
        bfs [(0,0)] []
        UM.unsafeRead d (idx (h-1) (w-1))

