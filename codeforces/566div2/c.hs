{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
#ifndef DEBUG
{-# LANGUAGE Safe              #-}
#endif

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Array                 as A
import qualified Data.Array.Unboxed         as UA
import qualified Data.Array.ST.Safe         as MA
import           Data.Bool
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import qualified Data.List                  as L
import qualified Data.List.NonEmpty         as NL
import           Data.Monoid
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import qualified Data.Set                   as S
import           Data.Tuple
import           Foreign
import qualified System.IO                  as IO
#ifdef DEBUG
import           Debug.Trace
#endif

main :: IO ()
main = do
  !n <- readLn :: IO Int
  bss <- C.lines <$> C.getContents
  let (m, b) = solve n bss
  print m
  B.hPutBuilder IO.stdout b

solve :: Int -> [C.ByteString] -> (Int, B.Builder)
solve n bss = (,) m $ construct m
  where
    dict :: A.Array Int C.ByteString
    !dict = A.listArray (0, n-1) bss

    encoded = zipWith encode [0..] bss
    (vow0, vows) = L.partition ((==0).fst) encoded

    sorted :: [(Int, Int)]
    !sorted = L.sort vows

    !m = upperBound 0 (div n 4) ok

    ok :: Int -> Bool
    ok k = go k [] $ map fst sorted
      where
        go 0 stock xs
          = (== k) . length . take k
          . pairs (==)
          . map (flip quot 10) $ map fst vow0 ++ reverse stock ++ xs
        go !acc !stock (x:y:xs)
          | x == y = go (acc - 1) stock xs
          | otherwise = go acc (x:stock) (y:xs)
        go _ _ _ = False

    construct :: Int -> B.Builder
    construct k = go2 k [] [] sorted
      where
        go2 0 stock ws xs = mconcat . zipWith merge ws
          . go1 k $ vow0 ++ reverse stock ++ xs
        go2 acc stock ws ((x,i):(y,j):xs)
          | x == y = go2 (acc-1) stock ((i, j):ws) xs
          | otherwise = go2 acc ((x,i):stock) ws ((y,j):xs)
        go1 0 _ = []
        go1 !acc ((x, i):(y, j):xs)
          | quot x 10 == quot y 10 = (i, j) : go1 (acc - 1) xs
          | otherwise = go1 acc ((y, j):xs)

        merge (x2, y2) (x1, y1)
          = mconcat
            [ B.byteString bsx1
            , B.char7 ' '
            , B.byteString bsx2
            , B.char7 '\n'
            , B.byteString bsy1
            , B.char7 ' '
            , B.byteString bsy2
            , B.char7 '\n'
            ]
          where
            bsx1 = dict A.! x1
            bsx2 = dict A.! x2
            bsy1 = dict A.! y1
            bsy2 = dict A.! y2

pairs :: (a -> a -> Bool) -> [a] -> [(a, a)]
pairs eq (x:y:xs)
  | eq x y = (x, y) : pairs eq xs
  | otherwise = pairs eq (y:xs)
pairs eq _ = []

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

encode :: Int -> C.ByteString -> (Int, Int)
encode i bs
    | len == 0 = (0, i)
    | otherwise = case C.last vowels of
      'a' -> (10 * len + 1, i)
      'e' -> (10 * len + 2, i)
      'i' -> (10 * len + 3, i)
      'o' -> (10 * len + 4, i)
      'u' -> (10 * len + 5, i)
  where
    vowels = C.filter isVowel bs
    len = C.length vowels

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        h = toInteger high
        l = toInteger low
        mid = fromIntegral $ l + div (h - l) 2
{-# INLINE lowerBound #-}

upperBound :: (Integral i) => i -> i -> (i -> Bool) -> i
upperBound low high p
    | p high = high
    | otherwise = lowerBound low high (not.p) - 1
{-# INLINE upperBound #-}
