import Data.Char

partitionAll :: Int -> Int -> [a] -> [[a]]
partitionAll _ _ [] = []
partitionAll w s xs = (take w xs) : partitionAll w s (drop s xs)

largestProductInSeries :: Int -> [Int] -> Int
largestProductInSeries l xs = maximum $ map (product) $ partitionAll l 1 xs

main :: IO ()
main = do
  f <- readFile "008.txt"
  let xs = map digitToInt (filter isDigit f)
  print $ largestProductInSeries 13 xs
