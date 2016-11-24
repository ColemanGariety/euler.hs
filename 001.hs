sumMultiplesOf3Or5 :: Int -> Int
sumMultiplesOf3Or5 n = sum [x | x <- [1..(n-1)], mod x 3 == 0 || mod x 5 == 0]
main = print (sumMultiplesOf3Or5 1000)
