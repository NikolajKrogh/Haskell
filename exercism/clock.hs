-- | Represents a clock with hours and minutes.
data Clock = Clock Int Int

-- | Equality instance for Clock.
-- Two clocks are considered equal if their normalized hours and minutes are equal.
instance Eq Clock where
  Clock h1 m1 == Clock h2 m2 = normalize h1 m1 == normalize h2 m2
    where
      -- Normalize the hours and minutes of a clock.
      normalize h m = let totalMin = h * 60 + m in ((totalMin `div` 60) `mod` 24, totalMin `mod` 60)

-- | Creates a Clock from the given hour and minute values.
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock hour min

-- | Converts a Clock to a string representation in the format "HH:MM".
toString :: Clock -> String
toString (Clock hour minutes) = format ((hour + minutes `div` 60) `mod` 24) ++ ":" ++ format (minutes `mod` 60)
  where
    -- Formats a single digit number with leading zero.
    format x = if x < 10 then "0" ++ show x else show x

-- | Adds the given hour and minute deltas to a Clock and returns the updated Clock.
addDelta :: Int -> Int -> Clock -> Clock
addDelta hourDelta minDelta (Clock hour minutes) = Clock newHour newMin
  where
    totalMin = (hour * 60 + minutes) + (hourDelta * 60 + minDelta)
    newHour = (totalMin `mod` 1440) `div` 60
    newMin = totalMin `mod` 60
