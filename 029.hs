import Data.Set

distinctPowers :: Integer -> Integer
distinctPowers cap = toInteger. size $ fromList [a^b | a <- [2..cap], b <- [2..cap]]

main = print . distinctPowers $ 100
