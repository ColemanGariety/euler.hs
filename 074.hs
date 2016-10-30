digits :: Integral x => x -> [x]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

fact n = product [1..n]

factDigitSum n = sum $ map (fact) (digits n)

genChain s = go [s]
  where go chain =
          let next = factDigitSum . last $ chain in
          if (elem next chain)
          then chain ++ [next]
          else go (chain ++ [next])

digitFactChains cap len = length [x | x <- [1..cap], length (genChain x) == len]
               
main = print $ digitFactChains 10 2
