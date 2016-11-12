import Data.Numbers.Primes

totientMaximum cap = last . fst . span (\a -> a < cap) $ scanl1 (*) (primes)

main = print $ totientMaximum 1000000
