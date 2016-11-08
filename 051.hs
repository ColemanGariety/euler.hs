import Data.Numbers.Primes

-- this one is forwards
fromDigits :: Integral x => [x] -> x
fromDigits = foldl (\b a -> (10 * b) + a) 0

-- this one is fowards
toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (a) ++ [b]
  where (a, b) = quotRem x 10

replaceDigits ds xs = map replace [0..9]
  where replace n = map (\(i, a) -> if elem i ds then n else a) (zip [0..(length xs)] xs)

length' n = length (show n)

primeDigitReplacements n = take 1 digitalPrimes
  where digitalPrimes = [toDigits x | x <- [56003]]

main = print . primeDigitReplacements $ 7
