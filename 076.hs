build x = (map sum (zipWith drop [0..] x) ++ [1]) : x
main = print $ (sum $ head $ iterate build [] !! 100) - 1
