every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
               (y:ys) -> y : every n ys
               [] -> []

isLeapYear :: Integral a => a -> Bool
isLeapYear year = if mod year 100 == 0 then mod year 400 == 0 else mod year 4 == 0

days :: (Num a1, Num t, Integral a, Eq a1) => a1 -> a -> t
days month year
  | month == 2 = if isLeapYear year then 29 else 28
  | elem month [9, 4, 6, 11] = 30
  | otherwise = 31

dates :: (Num t2, Num t1, Integral t, Eq t1, Enum t2, Enum t1) => t -> t -> [(t, t1, t2)]
dates from to = [(year, month, day) | year <- [from..to], month <- [1..12], day <- [1..(days month year)]]

countFirstSundays :: Integral t => t -> t -> Int
countFirstSundays from to = length . filter isFirst . dropWhile beforeFrom $ (every 7 (dates 1900 to))
  where isFirst (year, month, day) = day == 1
        beforeFrom (year, month, day) = year < from

main :: IO ()
main = print $ countFirstSundays 1901 2000
