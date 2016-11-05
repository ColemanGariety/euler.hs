isInt :: RealFrac a => a -> Bool
isInt n = fromInteger (round n) == n

unpenta :: Floating a => a -> a
unpenta n = ((sqrt (1 + (24 * n)) + 1) / 6)

hexas :: [Double]
hexas = [n * ((n * 2) - 1) | n <- [1..]]

-- Every hexa is also a triangle
-- so we check the pentaHexas above 40755
pentaHexaTriangles :: Integral t => Double -> [t]
pentaHexaTriangles from = [round h | h <- (dropWhile (<=from) hexas),
                           isInt . unpenta $ h]

main :: IO ()
main = print . head . pentaHexaTriangles $ 40755
