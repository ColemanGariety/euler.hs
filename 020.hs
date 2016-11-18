import Data.Tuple
import Data.List

-- this one i spurposefully backwards for speed
toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

factorial n = product [1..n]

main = print . sum . toDigits $ factorial 100
