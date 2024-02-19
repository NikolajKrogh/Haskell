import Data.Maybe (listToMaybe)

type Target = Integer
type Coins = [Integer]

findFewestCoins :: Target -> Coins -> Maybe Coins
findFewestCoins target coins = listToMaybe $ findFor target coins

findFor :: Target -> Coins -> [Coins]
findFor _ [] = []
findFor target coins@(c : cs)
  | target < 0 = []
  | target == 0 = [[]]
  | otherwise = findFor target cs <> ((c :) <$> findFor (target - c) coins)

  {-
  coins@(c : cs) is an as-pattern. 
  This pattern matches a list of coins and binds c to the head of the list (the first coin)
  and cs to the tail of the list (all the other coins). At the same time, coins is bound to the whole list. 
  This allows the code to use coins to refer to the whole list and c and cs to refer to the first coin
  and the remaining coins, respectively.
  -}