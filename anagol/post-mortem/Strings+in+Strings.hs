import Data.List
m@main=getLine>>=putStrLn.min"YES".foldl1(#).words>>m
x#y|isInfixOf x y=y|0<1="NO"