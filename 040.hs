import Data.List
import Data.Tuple

needs = [1,10,100,1000,10000,100000,1000000] 

numberDigits = map (\(a, b) -> [b]) . filter (\(a,b) -> elem a needs) $ xs
  where xs = zip [1..] . concat $ [(show x) | x <- [1..]]
        
main = print . product . map (read::String->Int) $ (take (length needs) $ numberDigits)
