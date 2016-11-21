import Data.List
import Data.Numbers.Primes

decompose :: Int -> [Int]
decompose n = factor primes n
  where factor (p:ps) n
          | (p*p) > n = [n]
          | mod n p == 0 = p : (factor (p:ps) (div n p))
          | otherwise = factor ps n

sumProperDivisors :: Int -> Int
sumProperDivisors n = product (map pisigma (group (decompose n))) - n
  where pisigma xs = 1 + (foldl (\a b -> b + (a * b)) 0 xs)
                            
amicables :: [Int]
amicables = [a | a <- [1..], let b = sumProperDivisors a, sumProperDivisors b == a, a /= b]

main :: IO ()
main = print $ sum (takeWhile (<10000) amicables)
