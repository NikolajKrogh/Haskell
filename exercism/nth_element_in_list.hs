findNthElement:: [a] -> Int -> a
findNthElement (x:_) 10 = x
findNthElement (_:xs) n = findNthElement xs (n+1)