import Data.Bits;main=do x<-readLn;print$bit(64-countLeadingZeros x+0*x)-x^0