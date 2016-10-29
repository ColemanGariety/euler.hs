import Data.Numbers.Primes

truncatablePrimes = sum $ take 11 [x | x <- (drop 4 primes),
                                       all (isPrime) (truncl (show x)),
                                       all (isPrime) (truncr (show x))]
  where truncl [] = []
        truncl xs = read xs : truncl (tail xs) 
        truncr [] = []
        truncr xs = read xs : truncr (init xs)

main = print truncatablePrimes
