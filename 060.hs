import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Testing

solve = do
 a <- primesTo10000
 let m = f a $ dropWhile (<= a) primesTo10000
 b <- m
 let n = f b $ dropWhile (<= b) m
 c <- n
 let o = f c $ dropWhile (<= c) n
 d <- o
 let p = f d $ dropWhile (<= d) o
 e <- p
 return [a,b,c,d,e]
 where
 f x = filter (\y -> and [isPrime $read $shows x $show y,
                          isPrime $read $shows y $show x])
       
primesTo10000 = 2 : filter isPrime [3,5..9999]

main = print $sum $head $solve
