import           Data.List
main=getLine>>=putStrLn.f
f s|"AC" `isInfixOf` s = "Yes"|0<1="No"
