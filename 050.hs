import Data.Numbers.Primes
import Data.Ord
import Data.List

consecutivePrimeSum limit = fst . maximumBy (comparing snd) . takeWhile (\(p, l) -> p < limit) $ zs
  where zs = [((ps!!x) - (ps!!y), x - y) | x <- [3..limit], y <- [x,(x-1)..3]] 
        ps = scanl (+) 0 (take limit primes)

main :: IO ()
main = print . consecutivePrimeSum $ 1000000
