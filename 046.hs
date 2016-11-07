import Data.Numbers.Primes
import Debug.Trace

isEven n = ((n `div` 2) * 2) == n

goldbach x = take 1 [(p, n) | p <- (take limit primes), n <- [1..limit],
                      let s = n^2,
                      p + (s * 2) == x]
  where limit = 577

goldbachRejection = head misses
  where (hits, misses) = span (\a -> not (null (goldbach a))) composites
        composites = [x | x <- [9..], not (isPrime x), not (isEven x)]

main = print $ goldbachRejection
