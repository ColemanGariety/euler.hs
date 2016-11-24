fibs :: [Int]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

evenFibs :: Int -> [Int]
evenFibs n = filter even (takeWhile (<= n) fibs)

main :: IO ()
main = print (sum (evenFibs 4000000))
