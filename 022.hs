import Data.List
import Data.List.Split
import Data.Char
import Data.Ord

scoreNames names = sum $ zipWith score (sort names) [1..]
  where score s i = (i *) . sum . map (\l -> ord l - ord 'A' + 1) $ s 

main = do
  f <- readFile "022.txt"
  print . scoreNames $ map (filter isLetter) (splitOn "," f)
