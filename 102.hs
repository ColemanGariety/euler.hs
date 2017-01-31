import Data.List.Split

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x,y)

areaFromPoints :: (Integral a1, Fractional a) => [(a1, a1)] -> a
areaFromPoints [(xa, ya), (xb, yb), (xc, yc)] = fromIntegral (abs prod) / 2
  where prod = (((xa - xc) * (yb - ya)) - ((xa - xb) * (yc - ya)))

pointsContainOrigin points@[(xa, ya), (xb, yb), (xc, yc)] = areaFromPoints points == areaFromPoints x + areaFromPoints y + areaFromPoints z
  where x = [(xa, ya), (xb, yb), (0, 0)]
        y = [(xa, ya), (0, 0), (xc, yc)]
        z = [(0, 0), (xb, yb), (xc, yc)]

main = do
  f <- readFile "102.txt"
  let lns = map (\x -> map tuplify (chunksOf 2 (map (read::String->Int) (splitOn "," x)))) $ lines f
  print . length . filter id $ map pointsContainOrigin lns
