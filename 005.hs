main = print $ smallestDivisibleBy [1..21]

smallestDivisibleBy :: [Integer] -> Integer
smallestDivisibleBy xs = foldl lcm 1 xs
