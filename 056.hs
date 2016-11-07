import Data.List
import Data.Tuple

main = print . maximum $ [sum (toDigits (a^b)) | a <- [1..100], b <- [1..100]]
  where toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10))) 
