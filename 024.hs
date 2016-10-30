import Data.List 

factorial n = product [1..n]
permutations' [] _ = []
permutations' xs n = x : permutations' (delete x xs) (mod n m)
  where m = factorial $ length xs - 1
        y = div n m
        x = xs !! y
 
main = print $ permutations' "0123456789" 999999
