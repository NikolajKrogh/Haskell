import Data.Char (digitToInt, isDigit)

-- | Convert a string of digits to a list of integers.
--
-- This function takes a string and converts it to a list of integers.
-- It does this by mapping the 'digitToInt' function over each character in the string, which converts each character to its corresponding integer value.
-- The resulting list of integers represents the digits in the string.
toDigits :: String -> [Integer]
toDigits = map (toInteger . digitToInt) . filter isDigit

-- | Double every other element in a list of integers, starting from the right.
--
-- This function takes a list of integers and doubles every other element, starting from the right.
-- It does this by using the 'zipWith' function to pair each element with its index, and then using the 'mod' function to determine if the index is odd.
--  If the index is odd, the element is doubled; otherwise, it remains unchanged.
-- The resulting list represents the doubled elements.
doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight = reverse . doubleEveryOther . reverse

-- | Double every other element in a list of integers.
--
-- This function takes a list of integers and doubles every other element.
-- It does this by reversing the list, using the 'doubleEveryOtherFromRight' function to double every other element starting from the right, and then reversing the resulting list back to its original order.
-- The resulting list represents the doubled elements.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id, (* 2)])

-- | Calculate the sum of the digits in a list of integers.
--
-- This function takes a list of integers and calculates the sum of the digits.
-- It does this by converting each integer to a string using the 'show' function, concatenating the resulting strings, mapping the 'digitToInt' function over the concatenated string to convert each character to its corresponding integer value, and finally calculating the sum of the resulting list of integers.
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap (toDigits . show)

onlyOneZero :: (Eq a, Num a) => [a] -> Bool
onlyOneZero xs = length xs == 1 && all (== 0) xs

-- | Check if a string represents a valid number according to the Luhn algorithm.
--
-- This function takes a string and checks if it represents a valid number according to the Luhn algorithm. It does this by first converting the string to a list of digits using the 'toDigits' function. It then doubles every other digit in the list using the 'doubleEveryOther' function. After that, it calculates the sum of the digits using the 'sumDigits' function. If the sum is divisible by 10, the string represents a valid number; otherwise, it does not.
-- Then, it doubles every other digit starting from the right using the 'doubleEveryOtherFromRight' function.
-- After that, it calculates the sum of the digits using the 'sumDigits' function.
-- Finally, it checks if the sum is divisible by 10 and if there are no all-zero digits in the original list.
isValid :: String -> Bool
isValid n = not (onlyOneZero digits) && (sumDigits digits `mod` 10 == 0)
  where
    digits = doubleEveryOtherFromRight . toDigits $ n
