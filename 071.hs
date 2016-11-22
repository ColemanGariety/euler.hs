import Data.Ratio

farey :: Integral t => Ratio t -> Ratio t -> t -> t
farey a b cap = let a' = (numerator a + numerator b) % (denominator a + denominator b)
                in if denominator a' <= cap then farey a' b cap else numerator a
    
main :: IO ()
main = print $ farey (0%1) (3%7) 1000000
