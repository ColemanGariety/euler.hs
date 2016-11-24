import Data.List
import Debug.Trace

crack :: [String] -> String
crack [] = []
crack xs = first : crack rest where
    heads = map head xs
    reject = nub $ map (!! 1) $ filter ((> 1) . length) xs
    first = head $ nub $ filter (`notElem` reject) heads
    rest = filter ((> 0) . length) $ map (\ys'@(y:ys) -> if y == first then ys else ys') xs

main = do
  f <- readFile "079.txt"
  print . (read::String->Int) . crack . nub $ lines f
