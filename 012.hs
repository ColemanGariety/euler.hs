import Data.List
import Data.Numbers.Primes

-- 4200 -> [2, 2, 2, 3, 5, 5, 7]
-- turns a number in to the unique factors which compose it
decompose :: Int -> [Int]
decompose n = factor primes n
  where factor (p:ps) n
          | (p*p) > n = [n]
          | mod n p == 0 = p : (factor (p:ps) (div n p))
          | otherwise = factor ps n

-- [2, 2, 2, 3, 5, 5, 7] -> [1,2,1,3]
-- unzips the decomposed number into the counts
factorialCounts :: Eq a => [a] -> [Int]
factorialCounts xs = map length (group xs)

-- 4200 -> [2, 2, 2, 3, 5, 5, 7] -> [1,2,1,3] -> 48
-- gets the number of divisors for a number via decomposition + fact sig
tau :: Int -> Int
tau n = foldl (\a count -> a * (1 + count)) 1 (factorialCounts $ decompose n)

-- Incrememnt x by 1 until the number of divisors (by tau function) exceeds 500 
triangleWithDivisors :: Int -> Int
triangleWithDivisors cap = head . snd $ break (\x -> tau x > cap) [((x + 1) * x) `quot` 2 | x <- [1..]]

-- print the triangle with with more than 500 divisors
main = print . triangleWithDivisors $ 500
