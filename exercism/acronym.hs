import Data.Char (isAlpha, isLower, isSpace, isUpper, toUpper)

m :: (Char, Char) -> String
m (a, b) | not (isAlpha a || a == '\'') && isAlpha b = [toUpper b]
m (a, b) | isLower a && isUpper b = [toUpper b]
m _ = []


abbreviate :: String -> String
abbreviate s = concatMap m $ zip (" " <> s) s
