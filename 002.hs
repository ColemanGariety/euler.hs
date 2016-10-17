fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
evenFibs n = filter even (takeWhile (<= n) fibs)
main = print (sum (evenFibs 4000000))
