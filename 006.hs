sumSquareDifference :: Num a => [a] -> a
sumSquareDifference xs = ((sum xs)^2) - (sum (map (^2) xs))

main :: IO ()
main = print $ sumSquareDifference [0..100]
