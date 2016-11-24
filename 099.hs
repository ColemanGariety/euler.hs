import Data.List
import Data.List.Split
import Data.Ord

flipLog (b:e) = (head e) * (log b)
largestExponential xs = snd . maximum $ zip (map flipLog xs) [1..]
main = readFile "099.txt" >>= print . largestExponential . map (map (read::String->Float) . (splitOn ",")) . lines
