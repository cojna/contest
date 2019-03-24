import Data.List
main=getLine>>=print.maximum.map(length.fst.span(`elem`"ACGT")).tails