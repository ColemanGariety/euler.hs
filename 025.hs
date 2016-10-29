fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) (tail fibs) fibs
main = print $ length (takeWhile (\n -> n < 10^999) fibs) + 1
