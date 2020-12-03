main :: IO ()
main = do
    cs <- getLine
    putChar . snd $ maximum [(sum[1|'a'<-cs], 'a'),(sum[1|'b'<-cs], 'b'),(sum[1|'c'<-cs], 'c')]