import Data.List.Split

maximumPath triangle = head $ foldr1 (\xs ys -> (zipWith3 (\x y z -> x + (max y z)) xs ys) (tail ys)) triangle

main = do
  f <- readFile "067.txt"
  let triangle = map (\a -> map (read::String->Int) (splitOn " " a)) $ lines f
  print . maximumPath $ triangle
