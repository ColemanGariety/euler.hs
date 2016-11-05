isInt :: RealFrac a => a -> Bool
isInt n = fromInteger (round n) == n

penta :: Fractional a => a -> a
penta n = (n * ((n * 3) - 1)) / 2

unpenta :: Floating a => a -> a
unpenta n = ((sqrt (1 + (24 * n)) + 1) / 6)

pentas :: [Double]
pentas = [(penta x) | x <- [1..]]

minimize :: [Integer]
minimize = [round (abs $ k - j) | j <- pentas, k <- (take (floor ((unpenta j) / 2)) pentas),
            isInt (unpenta (j + k)),
            isInt (unpenta (j - k))]

main :: IO ()
main = print . head $ minimize
