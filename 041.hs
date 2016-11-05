import Data.List
import Data.Numbers.Primes
import Data.Char (intToDigit)

largestPandigitalPrime = maximum [ n' | d <- [3..9], n <- permute ['1'..intToDigit d],
                            let n' = read n, isPrime n']
    where
        permute "" = [""]
        permute str = [(x:xs)| x <- str, xs <- permute (delete x str)]
        
main = print largestPandigitalPrime
