import Data.List
import Data.Ratio

digits :: Integral x => x -> [x]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

ratios s f = take 4 [fromInteger x % fromInteger y | x <- [s..f],
                                                 y <- [s..f],
                                                 let xd = digits x,
                                                 let yd = digits y,
                                                 let xy = intersect xd yd,
                                                 let dividend = (fromInteger x / fromInteger y),
                                                 let uniqFloat xs = fromInteger ((xs \\ xy) !! 0),
                                                 xy /= [0],
                                                 length xy == 1,
                                                 (uniqFloat xd) / (uniqFloat yd) == dividend]


digitCancellingProductDenominator s f = denominator . product $ (take 4 (ratios s f)) 

main = print $ digitCancellingProductDenominator 10 99
