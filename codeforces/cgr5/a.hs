    {-# OPTIONS_GHC -O2 #-}
    {-# LANGUAGE BangPatterns, CPP, LambdaCase, MultiWayIf, TupleSections #-}
    {-# LANGUAGE ViewPatterns                                             #-}
    #ifndef DEBUG
    {-# LANGUAGE Safe #-}
    #endif
     
    import           Control.Applicative
    import           Control.Exception
    import           Control.Monad
    import           Control.Monad.State.Strict
    import qualified Data.Array                 as A
    import qualified Data.Array.ST.Safe         as MA
    import qualified Data.Array.Unboxed         as UA
    import           Data.Bool
    import qualified Data.ByteString.Builder    as B
    import qualified Data.ByteString.Char8      as C
    import           Data.Char
    import           Data.Function
    import qualified Data.IntMap.Strict         as IM
    import qualified Data.IntSet                as IS
    import qualified Data.List                  as L
    import qualified Data.List.NonEmpty         as NL
    import qualified Data.Map.Strict            as M
    import           Data.Monoid
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
        n <- readLn
        xs <- L.unfoldr (C.readInt.C.dropWhile isSpace) <$> C.getContents
        putStr.unlines.map show $ solve n xs
     
    solve :: Int -> [Int] -> [Int]
    solve n xs = go (sum $ map (`quot` 2) xs) xs
      where
        go 0 xs = map (`quot` 2) xs
        go acc (x:xs)
            | even x = quot x 2 : go acc xs
            | acc > 0, x < 0 = div x 2 : go (acc - 1) xs
            | acc < 0, x > 0 = quot (x + 1) 2 : go (acc + 1) xs
            | otherwise = quot x 2 : go acc xs
     