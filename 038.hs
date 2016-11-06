import Data.List
import Data.Tuple

toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

-- purposefully backwards
fromDigits :: Integral x => [x] -> x
fromDigits = foldr (\b a -> (10 * a) + b) 0

isPandigital xs = (sort xs) == [1,2,3,4,5,6,7,8,9]

panMultiples = [m | x <- (reverse [9234..9387]),
                let m = (toDigits (x * 2) ++ (toDigits x)),
                isPandigital m]

main = print . fromDigits . head $ panMultiples
