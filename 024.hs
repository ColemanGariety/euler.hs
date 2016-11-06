import Data.List

-- this one is necesasrily forwards
fromDigits :: Integral x => [x] -> x
fromDigits = foldl (\b a -> (10 * b) + a) 0

factorial n = product [1..n]

permutations' [] _ = []
permutations' xs n = x : permutations' (delete x xs) (snd y)
  where m = factorial $ length xs - 1
        y = quotRem n m
        x = xs !! (fst y)

main = print . fromDigits $ permutations' [0,1,2,3,4,5,6,7,8,9] 999999
