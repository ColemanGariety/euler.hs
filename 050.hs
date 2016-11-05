-- soooo slow!

import Data.Numbers.Primes
import Data.List
import Data.Ord

findPrimes start = try start 0
  where try n from = if last res == n then length res else try n (succ from)
          where res = takeWhile (<=n) (scanl (+) 0 (drop from primes))

tryPrimes below = fst . maximumBy (comparing snd) $ [(x, findPrimes x) | x <- (takeWhile (<below) primes)]

main = print $ tryPrimes 1000000
