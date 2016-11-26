digitsOfSum :: (Foldable t1, Integral t) => t -> t1 String -> Integer
digitsOfSum n a = go sum
  where go x
          | x < 10^n = x
          | otherwise = go (quot x 10)
        sum = foldl (\a b -> a + (read b :: Integer)) 0 a

main :: IO ()
main = do
  f <- readFile "013.txt"
  print $ digitsOfSum 10 (lines f)
