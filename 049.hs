import Data.Numbers.Primes
import Data.List
import Data.Tuple

-- this one i spurposefully backwards for speed
toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

-- this one is purposefully backwards for speed
fromDigits :: Integral x => [x] -> x
fromDigits = foldr (\b a -> (10 * a) + b) 0

primePermutations = fromDigits ([(toDigits z) ++ (toDigits y) ++ (toDigits x) | x <- ps,
                                 let y = x + 3330,
                                 let z = x + (3330 * 2),
                                 let perms = permutations (toDigits x),
                                 isPrime y,
                                 isPrime z,
                                 elem (toDigits y) perms,
                                 elem (toDigits z) perms,
                                 x /= 1487] !! 0)
  where ps = (dropWhile (<1000) (takeWhile (<10000) primes))

main = print $ primePermutations
