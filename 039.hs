import Data.List
import Data.Ord

mode :: Ord a => [a] -> a
mode = head . maximumBy (comparing length) . group . sort

isInt n = n == (fromInteger (round n))

integerRightTriangles cap =  mode [p | a <- [1..cap / 2], b <- [1..(cap / 2)],
                                   let c = sqrt (a^2 + b^2),
                                   let p = a + b + c,
                                   isInt c,
                                   p <= cap]

main = print . integerRightTriangles $ 1000
