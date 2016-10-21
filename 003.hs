primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n
  | factors == [] = [n]
  | otherwise = factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
main = print (last (primeFactors 600851475143))
