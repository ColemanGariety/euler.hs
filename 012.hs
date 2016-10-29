-- 4200 -> [2, 2, 2, 3, 5, 5, 7]
-- turns a number in to the unique factors which compose it
decompose :: Int -> [Int]
decompose n = go 2 n
  where go divisor number
          | (divisor ^ 2) > number = [number]
          | (number `mod` divisor) == 0 = divisor : go divisor (number `quot` divisor)
          | otherwise = go (divisor + 1) number

-- [2, 2, 2, 3, 5, 5, 7] -> [1,2,1,3]
-- unzips the decomposed number into the counts
factorialCounts :: [Int] -> [Int]
factorialCounts (x:xs) = go x xs [1]
  where go m l r
          | null l = r
          | otherwise = go h (tail l) next
          where h = head l
                next = if h == m
                       then head r + 1 : tail r
                       else 1 : r

-- 4200 -> [2, 2, 2, 3, 5, 5, 7] -> [1,2,1,3] -> 48
-- gets the number of divisors for a number via decomposition + fact sig
tau :: Int -> Int
tau n = foldl (\a count -> a * (1 + count)) 1 (factorialCounts . decompose $ n)

-- Incrememnt x by 1 until the number of divisors (by tau function) exceeds 500 
triangleWithDivisors cap = [triangle | x <- [1..],
                            let triangle = ((x + 1) * x) `quot` 2,
                            tau triangle > cap] !! 0

-- print the triangle with with more than 500 divisors
main = print . triangleWithDivisors $ 500
