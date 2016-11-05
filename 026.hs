import Data.List
import Data.Ord
import Data.Fixed

reciprocalCycles cap = fst . maximumBy (comparing snd) $ ds
  where ds = [(x, (length $ cycle 1 x []) - 1) | x <- [2..cap]]
        cycle x y res = if elem d res then d : res else cycle (d*10) y (d:res)
          where d = mod' x y

main = print . reciprocalCycles $ 1000
