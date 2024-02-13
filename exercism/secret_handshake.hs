-- >>> handshake 1
-- ["wink"]
handshake :: Int -> [String]
handshake n 
    | n >= 16 = reverse actionsForBits
    | otherwise = actionsForBits
    where
        actions = ["wink", "double blink", "close your eyes", "jump"]
        toBinary 0 = []
        toBinary n = n `mod` 2 : toBinary (n `div` 2)
        actionsForBits = [action | (bit, action) <- zip (toBinary n) actions, bit == 1]



handshake2 :: Int -> [String]
handshake2 n | n >= 16 = reverse $ handshake2 (n-16)
            | n >=  8 = handshake2 (n-8) ++ ["jump"]
            | n >=  4 = handshake2 (n-4) ++ ["close your eyes"]
            | n >=  2 = handshake2 (n-2) ++ ["double blink"]
            | n >=  1 = handshake2 (n-1) ++ ["wink"]
            | otherwise = []