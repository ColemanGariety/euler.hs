import Data.Numbers.Primes
import Data.List
import Data.Ratio
import Data.Tuple
import Data.Ord

toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

phi 1 = 1
phi n = numerator ratio `div` denominator ratio
  where ratio = foldl (\acc x -> acc * (1 - (1 % x))) 
                (n % 1) $ nub (primeFactors n)

isPermutation as bs = sort (toDigits as) == sort (toDigits bs)

main = print . fst . minimumBy (comparing snd) $ xs
  where xs = [(n, (fromInteger n) / (fromInteger p)) | n <- [2..(10^7)],
               let p = phi n,
               isPermutation n p]

-- warning: 80 seconds to find solution
