import Math.NumberTheory.Primes.Testing
import Data.List
import Debug.Trace

val :: Maybe t -> t
val (Just x) = x

spiralPrimes :: Num a => t -> a
spiralPrimes n = lengths !! (val . findIndex (\(a,b) -> a/b < 0.1) $ (drop 2 ratios))
  where ratios = scanl (\(a,b) c -> (if isPrime c then a+1 else a, b+1)) (0,1) $ diagonals
        diagonals = scanl (+) 1 ([2,4..] >>= replicate 4)
        lengths = (1 : (zip (repeat 4) (iterate (2+) 3) >>= uncurry replicate))

main :: IO ()
main = print $ spiralPrimes 60
