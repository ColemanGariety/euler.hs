-- 4200 -> [2, 2, 2, 3, 5, 5, 7]
-- turns a number in to the unique factors which compose it
decompose :: Int -> [Int]
decompose n = go 2 n where
  go divisor number
    | (divisor ^ 2) > number = [number]
    | (number `mod` divisor) == 0 = divisor : go divisor (number `quot` divisor)
    | otherwise = go (divisor + 1) number

-- [2, 2, 2, 3, 5, 5, 7] -> [3,1,2,1]
-- unzips the decomposed number into the counts
factSig :: [Int] -> [Int]
factSig [] = []
factSig xs =
  let count = length $ filter (== (head xs)) xs
  in count : factSig (drop count xs)

-- 4200 -> [2, 2, 2, 3, 5, 5, 7] -> [3,1,2,1] -> 48
-- gets the number of divisors for a number via decomposition + fact sig
tau :: Int -> Int
tau n = foldl (\a count -> a * (1 + count)) 1 (factSig . decompose $ n)

triangle :: Int -> Int
triangle n = ((n + 1) * n) `quot` 2

triangleWithDivisors cap = go 1 where
 go x =
   let t = triangle x in
   if tau t > cap
     then t
     else go (x + 1)
   

-- print the triangle with with more than 500 divisors
main = print . triangleWithDivisors $ 500
