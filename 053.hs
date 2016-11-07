import Debug.Trace

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

c :: (Fractional a, Enum a) => a -> a -> a
c n r = (factorial n) / ((factorial r) * (factorial (n - r)))

combSelection ncap vmin = go 23 4 0
  where go n r count
          | n > ncap = count
          | v > vmin = go n (succ r) (succ count)
          | v == 0 = go (succ n) 4 count
          | otherwise = go n (succ r) count
          where v = c n r

main :: IO ()
main = print $ combSelection 100 1000000
