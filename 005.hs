smallestDivisibleBy :: [Integer] -> Integer
smallestDivisibleBy xs = foldl1 lcm xs

main :: IO ()
main = print $ smallestDivisibleBy [1..21]
