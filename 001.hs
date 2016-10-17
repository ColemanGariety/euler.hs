sumMultiplesOf3And5Below :: Int -> Int
sumMultiplesOf3And5Below n = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]
main = print (sumMultiplesOf3And5Below 1000)
