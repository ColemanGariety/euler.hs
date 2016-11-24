import Data.Numbers.Primes

decompose :: Int -> [Int]
decompose n = factor primes n
  where factor (p:ps) n
          | (p*p) > n = [n]
          | mod n p == 0 = p : (factor (p:ps) (div n p))
          | otherwise = factor ps n

main :: IO ()
main = print (last (decompose 600851475143))
