import Data.List

polygonal :: (RealFrac a, Integral b) => a -> a -> b
polygonal s n = round ((((n*n) * (s - 2)) - (n * (s - 4))) / 2)

polygonals :: (RealFrac a, Integral t, Enum a) => a -> [t]
polygonals s = [polygonal s n | n <- [1..]]

figurates :: [(Int, Int)]
figurates = concat [map (\a -> ((round s), a)) (dropWhile (<=10^3) (takeWhile (<10^4) (polygonals s))) | s <- [3..8]]

cyclicalChain :: (Integral a, Eq a1) => Int -> [(a1, a)] -> [a]
cyclicalChain size figurates@(r:rs) = go [r] rs []
  where go [] (r:rs) tried = go [r] rs []
        go chain@((s,l):cs) rem tried = if length chain == size && canClose chain
                                        then map snd . reverse $ chain
                                        else case find next figurates of
                                               Just n -> go (n:chain) rem tried
                                               Nothing -> go (tail chain) rem (snd (head chain) : tried)
          where next (y,x) = not (elem y (map fst chain)) &&
                             not (elem x tried) &&
                             truncr2 x == truncl2 l
        canClose chain = (truncr2 (snd (last chain))) == (truncl2 (snd (head chain)))
        truncl2 n = rem n 100
        truncr2 n = div n 100

main :: IO ()
main = print . sum $ cyclicalChain 6 (reverse figurates) -- reverse because there are fewer octagonals than triangles
