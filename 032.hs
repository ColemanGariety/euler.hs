import Data.List
import Data.Tuple

toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

isPandigital xs = (sort xs) == [1,2,3,4,5,6,7,8,9]

panProducts = [prod | m <- [2..100], n <- [(if m > 9 then 123 else 1234)..(10000 / (m + 1))],
               let prod = n * m,
               isPandigital ((toDigits (round prod)) ++ (toDigits (round n)) ++ (toDigits (round m)))]

main = print . sum . nub $ panProducts
