import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = [toLower x | x <- ['a' .. 'z'], x `elem` text] == ['a' .. 'z'] || [toLower x | x <- ['.'], x `elem` text] == ['.']

isPangram2 :: String -> Bool
isPangram2 text = all (`elem` map toLower text) ['a' .. 'z']