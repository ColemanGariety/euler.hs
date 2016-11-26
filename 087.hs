import Data.Numbers.Primes
import Data.Array.Unboxed
 
squares :: [Int]
squares = takeWhile (<50000000) [x^2 | x <- primes]
cubes :: [Int]
cubes   = takeWhile (<50000000) [x^3 | x <- primes]
fourths :: [Int]
fourths = takeWhile (<50000000) [x^4 | x <- primes]

expressible :: UArray Int Bool
expressible = accumArray (||) False (1, 50000000) [(a + b + c, True) | a <- squares,
                                                   b <- takeWhile (<(50000000-a)) cubes,
                                                   c <- takeWhile (<((50000000-a-b))) fourths]
 
main :: IO ()
main = print . length . filter id $ elems expressible
