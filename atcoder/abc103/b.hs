import           Data.List
main=interact$f.lines
f[s,t]|t`isInfixOf`(s++s)="Yes"|0<1="No"
