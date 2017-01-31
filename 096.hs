import Data.Array
import Data.List
import Data.List.Split
import Debug.Trace

rowAt :: (Ix a, Ix t2, Num t2, Enum t2) => (a, t) -> Array (a, t2) t1 -> [t1]
rowAt (x,_) a = [a ! (x,y) | y <- [0..8]]

colAt :: (Ix b, Ix t2, Num t2, Enum t2) => (t, b) -> Array (t2, b) t1 -> [t1]
colAt (_,y) a = [a ! (x,y) | x <- [0..8]]

boxAt :: (Ix t2, Ix t1, Integral t2, Integral t1) => (Integer, Integer) -> Array (t1, t2) t -> [t]
boxAt (x1,y1) a = [a ! (x2,y2) | x2 <- [sx..(sx+2)], y2 <- [sy..(sy+2)]]
  where sx = (floor ((fromInteger x1) / 3)) * 3
        sy = (floor ((fromInteger y1) / 3)) * 3

getCandidates :: (Num a, Eq a, Enum a) => (Integer, Integer) -> Array (Integer, Integer) a -> [a]
getCandidates (x,y) a = [1..9] \\ (filter (/= 0) (nub used))
  where used = (boxAt (x,y) a) ++ (rowAt (x,y) a) ++ (colAt (x,y) a)

sudoku f = go (0,0) (listArray ((0,0),(8,8)) f)
  where go (x,y) a
          | (traceShow (x,y) x) == 8 && y == 8 = a
          | isEmpty && (length cands) > 0 = go (nx, ny) (a // [((x,y), cands !! 0)])
          | (length cands) == 0 = go (px, py) a
          | otherwise = go (nx, ny) a
          where cands = getCandidates (x,y) a
                (nx, ny) = if x == 8 then (0, succ y) else (succ x, y)
                (px, py) = if x == 0 then (8, pred y) else (pred x, y)
                isEmpty = (a ! (x,y)) == 0

printArray :: Show a => Array i a -> IO ()
printArray arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf 9 (elems arr)

main :: IO ()
main = printArray $ sudoku [3,0,0,2,0,0,0,0,0,
                            0,0,0,1,0,7,0,0,0,
                            7,0,6,0,3,0,5,0,0,
                            0,7,0,0,0,9,0,8,0,
                            9,0,0,0,2,0,0,0,4,
                            0,1,0,8,0,0,0,5,0,
                            0,0,9,0,4,0,3,0,1,
                            0,0,0,7,0,2,0,0,0,
                            0,0,0,0,0,8,0,0,6]
