import Data.List
import Data.Tuple
import qualified Data.IntSet as Set
import qualified Data.Vector.Unboxed as Vector

digitFacts :: Vector.Vector Int
digitFacts = Vector.fromList ([1,1,2,6,24,120,720,5040,40320,362880] :: [Int])

factDigitSum = foldl' (+) 0 . map (digitFacts Vector.!) . unfoldr (\x -> if x == 0
                                                                         then Nothing
                                                                         else Just (swap (quotRem x 10)))

genChain :: Int -> Set.IntSet
genChain s = go s Set.empty where
  go link chain =
    let next = factDigitSum link in
    if Set.member next chain
    then chain
    else go next (Set.insert next chain)

digitFactChains :: Int -> Int -> Int
digitFactChains cap len = length [x | x <- [1..cap], Set.size (genChain x) == len - 1]
               
main = print $  digitFactChains 1000000 60
