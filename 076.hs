main = print . length $ [1 | a <- [5,(5-4)..0], b <- [a,(a-3)..0], c <- [b,(b-2)..0]]
