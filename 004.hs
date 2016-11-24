import Data.List

isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in reverse s == s

largestPalindrome :: Integer -> Integer
largestPalindrome e = maximum $ filter isPalindrome [x * y | x <- [100..10^e], y <- [100..10^e], y >= x]

main :: IO ()
main = print $ largestPalindrome 3
