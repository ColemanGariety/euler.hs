import Data.List.Split

maximumPathSum triangle = head (foldr1 a triangle)
  where a xs ys = zipWith3 b xs ys (tail ys)
        b x y z = x + (max y z)

main = do
  f <- readFile "018.txt"
  let triangle = map (\a -> map (read::String->Int) (words a)) $ lines f
  print . maximumPathSum $ triangle
