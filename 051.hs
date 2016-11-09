import Data.Numbers.Primes
import Data.List
import Data.Ord

-- this one is forwards
fromDigits :: Integral x => [x] -> x
fromDigits = foldl (\b a -> (10 * b) + a) 0

-- this one is fowards
toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (a) ++ [b]
  where (a, b) = quotRem x 10

replaceDigits ds xs = map replace [0..9]
  where replace n = map (\(i, a) -> if elem i ds
                                    then n
                                    else a) (zip [1..(length xs)] xs)

primeDigitReplacements n = head . head $ misses
  where (hits, misses) = span (\f -> length f /= n) . concat $ primeFamilies
        primeFamilies = [families . toDigits $ x | x <- primes]
        families xs = [filter (numbers xs) . map fromDigits $ replaceDigits ds xs |
                       ds <- (subsequences [1..(length xs)])]
        numbers xs b = (length (show b)) == (length xs) && isPrime b

main = print . primeDigitReplacements $ 8
