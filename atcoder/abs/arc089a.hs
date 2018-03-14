import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Unsafe     as B
import           Data.Char
import qualified Data.Vector.Unboxed        as U

main :: IO ()
main = do
    n <- readLn :: IO Int
    txys <- U.unfoldrN n parseInt3 <$> B.getContents
    putStrLn . bool "No" "Yes" $ solve txys

solve :: U.Vector (Int, Int, Int) -> Bool
solve txys = U.all p . U.zipWith (#) txys $ U.cons (0, 0, 0) txys
  where
    p (dt, dx, dy) = dx + dy  <= dt && even (dt - dx - dy)
    (t, x, y) # (pt, px, py) = (abs $ t - pt, abs $ x - px, abs $ y - py)

type Parser a = B.ByteString -> Maybe (a, B.ByteString)

parseInt :: Parser Int
parseInt = B.readInt . B.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)

parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (B.readInt . B.dropWhile isSpace)
        <*> StateT (B.readInt . B.unsafeTail)
        <*> StateT (B.readInt . B.unsafeTail)
