main = print $ sumSquareDifference [0..100]
sumSquareDifference xs = (sum xs)^2 - sum (map (^2) xs)
