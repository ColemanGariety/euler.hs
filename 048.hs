selfPowersDigits n cap = read (drop (length r - n) r) :: Int
  where r = show $ sum [x^x | x <- [1..cap]]

main = print $ selfPowersDigits 10 1000
