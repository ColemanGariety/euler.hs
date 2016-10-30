import Data.Numbers.Primes

truncl :: Integral t => t -> [t]
truncl 0 = []
truncl n = n : truncl (mod n (10 ^ (floor (logBase 10 (fromIntegral n)))))

truncr :: Integral t => t -> [t]
truncr 0 = []
truncr n = n : truncr (div n 10)

truncatablePrimes :: Integer
truncatablePrimes = sum $ take 11 [x | x <- (drop 4 primes),
                                       all (isPrime) (truncl x),
                                       all (isPrime) (truncr x)]
main = print truncatablePrimes
