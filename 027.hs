trials n = -(2 * (a - 1)) * (a^2 - (a + 41))
  where m = head $ filter (\x -> x^2 - x + 41 > n) [1..]
        a = m - 1

main = print . trials $ 1000
