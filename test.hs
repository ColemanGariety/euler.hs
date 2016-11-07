countSqs :: Int -> Int
countSqs n = 
    let y = sqrt (fromIntegral n)
        m = y in
    length $ [x | x <- [1..m], (n `mod` (x^2)) == 0]

main = print $ countSqs 10
