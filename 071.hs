orderedFractions :: RealFrac a => a -> (Integer, a) -> a
orderedFractions cap (a,b) = (fromInteger (((((floor ((cap - 5) / b)) * (round b)) + 5) * a) - 1)) / b

main :: IO ()
main = print . round $ orderedFractions 1000000 (3,7)
