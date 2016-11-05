isInt :: RealFrac a => a -> Bool
isInt n = fromInteger (round n) == n

triangles :: [Double]
triangles = [(n * (n + 1)) / 2 | n <- [1..]]

unpenta :: Floating a => a -> a
unpenta n = ((sqrt (1 + (24 * n)) + 1) / 6)

unhexa :: Floating a => a -> a
unhexa n = ((sqrt ((8 * n) + 1)) + 1) / 4

pentaHexaTriangles :: Integral t => Int -> [t]
pentaHexaTriangles from = [round t | t <- (drop from triangles),
                           isInt . unpenta $ t,
                           isInt . unhexa $ t]

main :: IO ()
main = print . head . pentaHexaTriangles $ 285
