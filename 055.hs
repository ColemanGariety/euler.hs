import Data.List
import Data.Tuple

toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

fromDigits :: Integral x => [x] -> x
fromDigits = foldr (\b a -> (10 * a) + b) 0

isPalindromic n = ds == (reverse ds)
  where ds = toDigits n

reverse' n = fromDigits (reverse (toDigits n))

isLychrel start = go start 0
  where go n c
          | isPalindromic next = False
          | c == 50 = True
          | otherwise = go next (succ c)
          where next = n + (reverse' n)

main = print . length . filter isLychrel $ [1..9999]
