import Data.List
import Data.Numbers.Primes
import Data.Tuple

rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

rotations :: [a] -> [[a]]
rotations xs = take (length xs) (iterate rotate xs)

-- purposefully backwrads
toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

-- purposefully backwards
fromDigits :: Integral x => [x] -> x
fromDigits = foldr (\b a -> (10 * a) + b) 0
         
circularPrimes cap = length [x | x <- [2..(cap - 1)],
                                 all (\a -> isPrime (fromDigits a)) (rotations (toDigits x))]


main = print $ circularPrimes 1000000
