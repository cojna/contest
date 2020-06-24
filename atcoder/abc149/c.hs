{-# LANGUAGE MagicHash #-}

import GHC.Exts
import GHC.Integer.GMP.Internals

main :: IO ()
main = do
  W# w# <- readLn :: IO Word
  print $ W# (nextPrimeWord# (minusWord# w# 1##))
