import Data.List
import Data.Array

limit = 28124

divisors n = (1:) $ nub $ concat [[x, div n x] | x <- [2..limit], rem n x == 0 ]
     where limit = (floor . sqrt . fromIntegral) n

isAbundant x = (>x) . sum . divisors $ x
abundants = listArray (1, limit) $ map isAbundant [1..limit]
rests x = map (x-) $ takeWhile (<= x `div` 2) (filter (abundants !) [1..limit]) 
isSum = any (abundants !) . rests
nonAbundantSums = sum . filter (not . isSum) $ [1..limit] 
main = print nonAbundantSums
