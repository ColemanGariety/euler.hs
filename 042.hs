import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace

triangles = [(n / 2) * (n + 1) | n <- [1..]]

codedTriangleWords ws = length [w | w <- ws, elem (fromIntegral (score w)) (take 18 triangles)]
  where score w = sum $ map (subtract 64 . ord) w

main = do
  f <- readFile "042.txt"
  print . codedTriangleWords $ map (filter isLetter) (splitOn "," f)
