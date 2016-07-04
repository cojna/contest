{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Data.Int
import           Data.Proxy
import           GHC.TypeLits

main :: IO ()
main = do
    [m, n] <- map read.words <$> getLine :: IO [Int]
    print $ (fromIntegral m :: IntMod) ^ n

type IntMod = GF (1000000007 :: Nat)

newtype GF (p :: Nat) = GF{ unGF :: Int64 } deriving Eq

modulus :: forall p. (KnownNat p) => GF p -> Int64
modulus _ = fromIntegral $ natVal (Proxy :: Proxy p)
{-# INLINE modulus #-}

instance Show (GF p) where
    show = show . unGF

instance (KnownNat p) => Num (GF p) where
    x + y = case modulus x of { m ->
            case unGF x + unGF y of { xy ->
                if xy < m then GF xy else GF (xy - m)
            }}
    x - y = case unGF x - unGF y of { xy ->
                if xy >= 0 then GF xy else GF (xy + modulus x)
            }
    x * y = GF $ unGF x * unGF y `rem` modulus x
    abs = fromIntegral . unGF
    signum = id
    fromInteger x = GF . fromIntegral $ x `mod` natVal (Proxy :: Proxy p)
