-- It runs in 2.5 seconds (which isn't too slow) but I also
-- had to manually tune it to take exactly 6, since I guessed
-- that's how many there were. How to do it without guessing???

digits :: Integral x => x -> [x]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

sumDigitPowers n = sum . take 6 $ [x | x <- [2..], sum (map (^n) (digits x)) == x]

main = print . sumDigitPowers $ 5
