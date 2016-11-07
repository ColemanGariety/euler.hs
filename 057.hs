import Data.Ratio
import Data.List
import Data.Tuple

converge :: Fractional t => t -> [t]
converge n = 1 + (1/n) : converge (2 + 1/n)

squareRootConvergents :: Integer -> [(Integer, Integer)]
squareRootConvergents n = (filter (\(n,d) -> length (show n) > length (show d)) expans)
  where expans = take (fromInteger n) . map (\a -> (numerator a, denominator a)) . converge $ 2 

main = print . length . squareRootConvergents $ 1000
