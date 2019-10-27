import Data.Char
main=interact(\(c:s)->toUpper c:map toLower s)