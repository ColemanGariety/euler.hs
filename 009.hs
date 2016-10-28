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
isInt sum = sum == fromInteger (round sum)

specialTriplet :: Integer -> Integer
specialTriplet sum0 = go a0 where
  a0 = fromInteger (quot sum0 3)
  sum = fromInteger sum0
  go a =
    let b = ((sum^2 / 2) - (sum * a)) / (sum - a) in
    if isInt b
      then round (a * b * (sum - b - a))
      else go (a - 1)

main = print $ specialTriplet 1000
