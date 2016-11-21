polygonal s n = (((n*n) * (s - 2)) - (n * (s - 4))) / 2

figurates = [polygonal s n | n <- [1..], s <- [1..8]]

main :: IO ()
main = print . take 100 $ figurates
