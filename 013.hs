digitsOfSum n a = go sum
  where go x
          | x < 10^n = x
          | otherwise = go (div x 10)
        sum = foldl (\a b -> a + (read b:: Integer)) 0 a

main = do
  f <- readFile "013.txt"
  print $ digitsOfSum 10 (lines f)
