import Data.List
import Data.Tuple
import Debug.Trace

isBouncy :: Integral a => a -> Bool
isBouncy n = go n False False
  where go n inc dec = let a = div (mod n 100) 10
                           b = mod n 10
                       in if n < 10
                          then inc && dec
                          else go (div n 10) (a > b || inc) (a < b || dec)

bouncyDensity :: (Ord a, Num a, Integral b, Enum a) => a -> b
bouncyDensity d = snd (head (snd (break (\(a,b) -> a >= (d * (fromIntegral b))) (zip [1..] (filter isBouncy [1..])))))

main = print (bouncyDensity 0.99)
