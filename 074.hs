import qualified Data.Set as Set

digits :: Integral x => x -> [x]
digits 0 = []
digits x = mod x 10 : digits (div x 10)

digitFacts = [1,1,2,6,24,120,720,5040,40320,362880]

factDigitSum n = sum $ map (digitFacts !!) (digits n)

genChain s = go s Set.empty where
  go link chain =
    let next = factDigitSum link in
    if Set.member next chain
    then chain
    else go next (Set.insert next chain)

digitFactChains cap len = length [x | x <- [1..cap], Set.size (genChain x) == len - 1]
               
main = print $  digitFactChains 1000000 60
