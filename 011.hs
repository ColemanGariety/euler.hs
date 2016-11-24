import Data.Array
import Debug.Trace

travel a = product . map (\(x,y) -> if inRange (bounds a) (x,y) then a ! (x,y) else 1)

se a (x,y) = travel a (zip [x..(x+3)] [y..(y+3)])
sw a (x,y) = travel a (zip (reverse [y..(y+3)]) [(x-3)..x])
s a (x,y) = travel a (zip [x..(x+3)] (replicate 4 y))
e a (x,y) = travel a (zip (replicate 4 x) [y..(y+3)])

largestInGrid f = maximum [maximum [se a (x,y), sw a (x,y), s a (x,y), e a (x,y)] | x <- [lx..hx], y <- [ly..hy]]
  where a = listArray ((0,0),(19,19)) f
        ((lx,ly),(hx,hy)) = bounds a

main = do
  f <- readFile "011.txt"
  print $ largestInGrid (map (read::String->Int) . words $ f)
