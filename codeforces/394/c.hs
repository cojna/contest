{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE FlexibleContexts  #-}


import Data.Array.Unboxed
import Data.Array.ST.Safe (STArray, STUArray)
import Data.Array.MArray.Safe
import Control.Applicative
import Control.Monad
import Control.Monad.ST.Safe
import Data.Bits
import Data.Maybe

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine
    css <- replicateM n getLine
    print $ solve n css

solve :: Int -> [String] -> Int
solve n css = fromJust $ minCostFlow (n+5) numE gr src sink 3
  where
    num = n
    alpha = n + 1
    symbol = n + 2
    src = n + 3
    sink = n + 4
    numE = 4 * n + 3
    gr = [(num, sink, 0, 1), (alpha,sink,0,1), (symbol,sink,0,1)]
      ++ [(src,i,0,1)|i<-[0..n-1]]
      ++ concat[[(i,num,x,1),(i,alpha,y,1),(i,symbol,z,1)]|(i,cs)<-zip[0..]css,let(x,y,z)=calc cs]

convert c
  | '0' <= c, c <= '9' = 0
  | 'a' <= c, c <= 'z' = 1
  | otherwise = 2

calc cs = go inf inf inf $ zip [0..] ns ++ zip [1..](reverse$tail ns)
  where
    ns = map convert cs
    go !x !y !z ((i,c):rest)
      | c == 0 = go (min i x) y z rest
      | c == 1 = go x (min i y) z rest
      | otherwise = go x y (min i z) rest
    go !x !y !z [] = (x, y, z)


type Vertex = Int
type Edge = Int
type Cost = Int
type Cap = Int

inf :: Cost
inf = 0x3f3f3f3f
{-# INLINE inf #-}

minCostFlow :: Int -> Int -> [(Vertex,Vertex,Cost,Cap)] -> Vertex -> Vertex -> Cap -> Maybe Cost
minCostFlow numv nume vvccs source sink flow0 = runST $ do
  gr        <- newArray  (0,numv-1) []     :: ST s (STArray  s Vertex [Edge])
  residual  <- newArray  (0,2*nume-1) 0    :: ST s (STUArray s Edge Cap)
  to        <- newArray_ (0,2*nume-1)      :: ST s (STUArray s Edge Vertex)
  cost      <- newArray_ (0,2*nume-1)      :: ST s (STUArray s Edge Cost)
  potential <- newArray  (0,numv-1) 0      :: ST s (STUArray s Vertex Cost)
  dist      <- newArray  (0,numv-1) inf    :: ST s (STUArray s Vertex Cost)
  prevv     <- newArray  (0,numv-1) (-1)   :: ST s (STUArray s Vertex Vertex)
  preve     <- newArray  (0,2*nume-1) (-1) :: ST s (STUArray s Vertex Edge)

  let mkGraph !i ((v,nv,c,cap):vvccs) = do
        modifyArray gr v (i:) >> modifyArray gr nv ((i+1):)
        writeArray to i nv    >> writeArray to (i+1) v
        writeArray cost i c   >> writeArray cost (i+1) (-c)
        writeArray residual i cap
        mkGraph (i+2) vvccs
      mkGraph _ _ = return ()

  let dijkstra (Fork v c hs) = do
        dv <- readArray dist v
        if c > dv then dijkstra $ mergePairs hs
        else do
          es <- readArray gr v
          hs' <- forM es $ \e -> do
            nv   <- readArray to e
            cap  <- readArray residual e
            hv   <- readArray potential v
            hnv  <- readArray potential nv
            v2nv <- readArray cost e
            old  <- readArray dist nv
            let dnv = dv + v2nv + hv - hnv
            if cap > 0 && dnv < old then do
              writeArray dist nv dnv
              writeArray prevv nv v
              writeArray preve nv e
              return $ Fork nv dnv []
            else return Empty
          dijkstra $ mergePairs hs `merge` mergePairs hs'
      dijkstra Empty = readArray dist sink

  let update flow = go sink flow
         where
           go !v !f = do
             pv <- readArray prevv v
             if pv < 0 then return f
             else do
               pv2v <- readArray preve v
               nf <- readArray residual pv2v >>= go pv . min f
               modifyArray residual pv2v (subtract nf)
               modifyArray residual (xor pv2v 1) (nf+)
               return nf

  let primalDual !flow !res
        | flow == 0 = return $ Just res
        | otherwise = do
            rep numv $ \i ->
                writeArray dist i inf
            writeArray dist source 0
            dsink <- dijkstra $ Fork source 0 []
            if dsink == inf then return Nothing
            else do
              rep numv $ \v-> do
                dv <- readArray dist v
                modifyArray potential v (dv+)
              f <- update flow
              hsink <- readArray potential sink
              primalDual (flow - f) $ hsink * f + res

  mkGraph 0 vvccs
  primalDual flow0 0

rep, rev :: Monad m => Int -> (Int -> m ()) -> m ()
rep n f=foldr((>>).f)(return())[0..n-1]
rev n f=foldr((>>).f.negate)(return())[1-n..0]
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for a b f=foldr((>>).f)(return())[a..b]
{-# INLINE rep #-}
{-# INLINE rev #-}
{-# INLINE for #-}

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f=readArray a i>>=writeArray a i.f
{-# INLINE modifyArray #-}

-- Pairing heap for Dijkstra
data Heap = Fork {-# UNPACK #-} !Vertex
                 {-# UNPACK #-} !Cost
                                [Heap]
          | Empty

merge :: Heap -> Heap -> Heap
merge hx@(Fork _ x _) hy@(Fork _ y _)
  | x <= y    = join hx hy
  | otherwise = join hy hx
 where
   join (Fork v cost hs) h = Fork v cost (h:hs)
merge Empty hy = hy
merge hx Empty = hx

mergePairs :: [Heap] -> Heap
mergePairs []       = Empty
mergePairs [x]      = x
mergePairs (x:y:hs) = merge (merge x y) (mergePairs hs)