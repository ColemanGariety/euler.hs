import Data.Array

travel a = [product xs | x <- [0..19],
                         y <- [0..19],
                         ]

main = do
  f <- readFile "011.txt"
  let a = listArray ((0,0),(19,19)) . map (read::String->Int) . words $ f
  print $ travel a
