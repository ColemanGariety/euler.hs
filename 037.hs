import Data.Numbers.Primes

truncatablePrimes = sum $ take 11 [x | x <- primes,
                                       all (isPrime) (truncl (show x)),
                                       all (isPrime) (truncr (show x)),
                                       not $ elem x [2,3,5,7]]
  where truncl [] = []
        truncl all = read all : truncl (tail all) 
        truncr [] = []
        truncr all = read all : truncr (init all)

main = print truncatablePrimes
