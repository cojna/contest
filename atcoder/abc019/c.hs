import Data.Set;main=getContents>>=print.size.fromList.fmap(f.read).tail.words;f x|odd x=x|0<1=f$div x 2