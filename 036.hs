import Data.List
import Data.Tuple
import Data.Char
import Numeric

toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

isPalindromic a = a == (reverse a)

doubleBasePalindromes cap = sum [x | x <- [1..(cap - 1)],
                                     let d = toDigits x,
                                     isPalindromic d,
                                     isPalindromic (showIntAtBase 2 intToDigit x "")]

main = print $ doubleBasePalindromes 1000000
