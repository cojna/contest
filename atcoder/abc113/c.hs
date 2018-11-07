{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Char
import           Data.List
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    [n, m] <- map read.words <$> getLine :: IO [Int]
    pys <- U.unfoldrN m parsePY <$> B.getContents
    putStr.unlines.map showAnswer.U.toList $ solve pys

solve :: U.Vector PY -> U.Vector IPJ
solve pys = radixSort64
    . U.postscanl' step 0
    . radixSort64
    $ U.imap (.|.) pys
  where
    step :: IPJ -> PYI -> IPJ
    step prev pyi
        | prevP == p = nextI .|. ((prev .&. 0x3ffffffff) + 1)
        | otherwise =  nextI .|. unsafeShiftL p 17 .|. 1
      where
        p = pyiToP pyi
        nextI = unsafeShiftL (pyiToI pyi) 34
        prevP = unsafeShiftR prev 17 .&. 0x1ffff

addLeadingZeros :: Int -> String
addLeadingZeros x = tail . show $ x + 1000000000000

showAnswer :: IPJ -> String
showAnswer ipj = addLeadingZeros $ (ipjToP ipj * 1000000) + ipjToJ ipj

type IPJ = Int -- I: 17, P: 17, J: 17
ipjToP :: IPJ -> Int
ipjToP ipj = unsafeShiftR ipj 17 .&. 0x1ffff

ipjToJ :: IPJ -> Int
ipjToJ ipj = ipj .&. 0x1ffff

type PY = Int
encodePY :: Int -> Int -> PY
encodePY p y = unsafeShiftL p 47 .|. unsafeShiftL y 17

type PYI = Int -- P: 17, Y: 30, I: 17
pyiToP :: PYI -> Int
pyiToP pyi = unsafeShiftR pyi 47 .&. 0x1ffff

pyiToI :: PYI -> Int
pyiToI pyi = pyi .&. 0x1ffff

type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parsePY :: Parser PY
parsePY = runStateT $
    encodePY
        <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)

radixSort64 :: U.Vector Int -> U.Vector Int
radixSort64 v = foldl' step v [0, 16, 32, 48]
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step v k = U.create $ do
        pref <- U.unsafeThaw
            . U.prescanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate 0x10000 0)
            $ U.map (flip (,) 1. mask k) v
        res <- UM.unsafeNew $ U.length v
        U.forM_ v $ \x -> do
            let !masked = mask k x
            i <- UM.unsafeRead pref masked
            UM.unsafeWrite pref masked $ i + 1
            UM.unsafeWrite res i x
        return res

