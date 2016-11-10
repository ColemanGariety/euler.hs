import Data.List
import Data.Tuple

toDigits :: Integral x => x -> [x]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (quotRem x 10)))

fromDigits :: Integral x => [x] -> x
fromDigits = foldr (\b a -> (10 * a) + b) 0

cubes :: [Integer]
cubes = map (\x -> x^3) [1..]

isCube :: Integer -> Bool
isCube n = n == (head $ dropWhile (<n) cubes) 

cubicPermutations cap = snd $ break hasCubePerms (take 1000 cubes)
  where hasCubePerms n = (==5) . length . filter isCube . nub . map fromDigits . permutations $ toDigits n

main = print $ cubicPermutations 5
