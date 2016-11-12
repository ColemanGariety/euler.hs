import Data.Bits
import Data.Char
import Data.List
import Data.List.Split

decrypt :: [Char] -> [Int] -> [Int]
decrypt key xs = map (\(a,b) -> xor a b) $ zip (cycle (map ord key)) xs

bruteDecrypt :: [Int] -> Int
bruteDecrypt xs = sum $ head misses where
  isEnglish xs = isInfixOf " the " (map chr xs)
  (hits, misses) = break isEnglish $ [decrypt [a,b,c] xs |
                                       a <- ['a'..'z'],
                                       b <- ['a'..'z'],
                                       c <- ['a'..'z']]

main = do
  f <- readFile "059.txt"
  print . bruteDecrypt . map (read::String->Int) . concatMap (splitOn ",") $ lines f 
