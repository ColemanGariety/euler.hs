import Data.List
import Data.Numbers.Primes

distinctPrimeFactors count = go (count ^ ((count * 2) - 1)) 0
  where go n c
          | c == count = (n - count)
          | l == count = go (succ n) (succ c)
          | otherwise = go (succ n) 0
            where l = length . nub . primeFactors $ n

main = print . distinctPrimeFactors $ 4
