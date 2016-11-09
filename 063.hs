digitCount :: Integer -> Int
digitCount = go 1 . abs
    where go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

powerfulDigitCounts :: Integer -> Int
powerfulDigitCounts limit = length [(a,b) | a <- [1..limit], b <- [1..limit], (toInteger (digitCount (a^b))) == (toInteger b)]

main :: IO ()
main = print $ powerfulDigitCounts 21
