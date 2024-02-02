import Data.Char

responseFor :: String -> String
responseFor xs 
  | isSilent = "Fine. Be that way!"
  | isShouting && isQuestion = "Calm down, I know what I'm doing!"
  | isQuestion = "Sure."
  | isShouting = "Whoa, chill out!"
  | otherwise = "Whatever."
  where 
    text = filter (not . isSpace) xs
    letters = filter isLetter xs
    isShouting = all isUpper letters && any isUpper letters
    isQuestion = last text == '?'
    isSilent = null xs || all isSpace xs