import Data.Numbers.Primes
import Data.Ord
import Data.List

consecutivePrimeSum limit = fst . maximumBy (comparing snd) . takeWhile (\(p, l) -> p < limit) $ zs
  where zs = [((ps!!x) - (ps!!y), x - y) | x <- [1..limit], y <- [(x-1),(x-2)..0]] 
        ps = scanl (+) 0 (drop 3 (take limit primes))

main :: IO ()
main = print . consecutivePrimeSum $ 1000000
