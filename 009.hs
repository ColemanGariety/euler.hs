-- -- From haskell.com:
-- 
-- specialTriplet sum = [[a,b,c] | m <- [2..limit],
--                                 n <- [1..(m-1)],
--                                 let a = m^2 - n^2,
--                                 let b = 2 * m * n,
--                                 let c = m^2 + n^2,
--                                 a + b + c == sum]
--   where limit = floor . sqrt . fromIntegral $ sum

-- My implementation is a zillion times faster:

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x) 

specialTriplet :: Integer -> Integer
specialTriplet sum =
  go (fromInteger (quot sum 3))
  where x = (fromInteger sum)
        go a
          | isInt b = round (a * b * (x - b - a))
          | otherwise = go (a - 1)
          where b = ((x^2 / 2) - (x * a)) / (x - a)

main = print $ specialTriplet 1000
