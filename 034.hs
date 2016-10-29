factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

digits :: Integral x => x -> [x]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

sumCurious = sum . take 2 $ [x | x <- [3..], (sum $ map (factorial) (digits x)) == x]

main = print sumCurious
