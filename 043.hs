import Data.List
import Data.Tuple

slice from to xs = take (to - from + 1) (drop from xs)

-- this one is necesasrily forwards
fromDigits :: Integral x => [x] -> x
fromDigits = foldl (\b a -> (10 * b) + a) 0

factorial n = product [1..n]

permutate [] _ = []
permutate xs n = x : permutate (delete x xs) (mod n m)
  where m = factorial $ length xs - 1
        y = div n m
        x = xs !! y

zeroToNinePermutations = [(fromDigits x) | n <- [0..(9 * (factorial 9))],
                          let x = permutate [0,1,2,3,4,5,6,7,8,9] n,
                          (fromDigits (slice 1 3 x)) `rem` 2 == 0,
                          (fromDigits (slice 2 4 x)) `rem` 3 == 0,
                          (fromDigits (slice 3 5 x)) `rem` 5 == 0,
                          (fromDigits (slice 4 6 x)) `rem` 7 == 0,
                          (fromDigits (slice 5 7 x)) `rem` 11 == 0,
                          (fromDigits (slice 6 8 x)) `rem` 13 == 0,
                          (fromDigits (slice 7 9 x)) `rem` 17 == 0]
 
main = print . sum $ (zeroToNinePermutations)
