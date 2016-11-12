latticePaths :: Num a => Int -> a
latticePaths size = iterate (scanl1 (+)) (repeat 1) !! size !! size

main :: IO ()
main = print $ latticePaths 20
