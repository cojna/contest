import Data.List;main=interact$f.zip[0..].words;f(_:l)|s<-sortOn((0-).read.snd)l=do x<-l;snd(snd(span(==x)s)!!0)++"\n"