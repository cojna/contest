import Data.Bool
import Data.List

main=getLine>>=putStrLn.bool"NO""YES".isK

keyence :: String
keyence = "keyence"

isK :: String -> Bool
isK cs = or $ do
    (k0, k1) <- zip (inits keyence) (tails keyence)
    return $ k0 `isPrefixOf` cs && k1 `isSuffixOf` cs
