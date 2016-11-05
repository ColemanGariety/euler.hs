digits :: Integral x => x -> [x]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

main = print . sum . digits $ 2^1000
