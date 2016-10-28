-- -- From haskell.com:
-- 
-- specialTriplet cap = [[a,b,c] | m <- [2..limit],
--                                 n <- [1..(m-1)],
--                                 let a = m^2 - n^2,
--                                 let b = 2 * m * n,
--                                 let c = m^2 + n^2,
--                                 a + b + c == cap]
--   where limit = floor . sqrt . fromIntegral $ cap

-- My implementation is a zillion times faster:

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

specialTriplet :: Int -> Int -> Int
specialTriplet n a
  | isInt (realToFrac b) = round (fromIntegral a * b * (fromIntegral n - b - fromIntegral a))
  | otherwise = specialTriplet n (a - 1)
  where b = ((fromIntegral n^2 / 2) - (fromIntegral n * (fromIntegral a))) / (fromIntegral n - fromIntegral a)

main = print $ specialTriplet 1000 (quot 1000 3)
