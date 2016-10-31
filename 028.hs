-- i = index
-- a = sum accumulator
-- x = target for next diagonal
-- y = target for next sublating diagonal
-- z = space between targets for current ring

sumSpiralDiag s = go 1 0 1 1 0
  where go i a x y z
          | i == (s^2 + 1) = a
          | i == y = go (succ i) (a + i) (x + (z + 2)) (y + ((z + 2) * 4)) (z + 2)
          | i == x = go (succ i) (a + i) (x + z) y z
          | otherwise = go (succ i) a x y z

main = print $ sumSpiralDiag 1001
