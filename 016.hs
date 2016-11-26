import Data.Tuple
import Data.List

toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

main :: IO ()
main = print . sum . toDigits $ 2^1000
