-- question 1 a
data Colour = Red | Green | Blue
  deriving (Eq, Show, Enum)

instance Bounded Colour where
  minBound = Red
  maxBound = Blue

-- question b
-- instance Enum Colour where
--   succ Green = Blue
  -- fromEnum Blue = 2
  -- toEnum 0 = Red
--   enumFrom Red = [Red, Green, Blue]

data AmPm = Am | Pm
  deriving (Enum, Show, Eq)

data Time = TwentyFour Int | Twelve Int Int AmPm

instance Show Time where
  show (TwentyFour t) = addZero h ++ ':':addZero m
  (h, m) = t `divMod` 100
  show (Twelve h m ampm)
    | h == 12 && ampm == Pm = "Midday"
    | h == 12 && ampm == Am = "Midday"
    | ampm == Am = time ++ "am"
    | ampm == Pm = time ++ "pm"
    where
      time :: String
      hour :: String
      minute :: String
      time = hour ++ ':':minute
      hour = addZero h
      minute = addZero m

addZero :: Int -> String
addZero int
  | int < 10 = '0':show int
  | otherwise = show int

-- it is also possible to change the show class of AmPm


instance Eq Time where
  (==) t1 t2
    = t1' == t2'
      where
        (TwentyFour t1') = to24 t1
        (TwentyFour t2') = to24 t2
-- seems to need to add zero, but don't care at the moment

{-
This is a multiple line comment!!!


-}


to24 :: Time -> Time
to24 (TwentyFour t)
  = TwentyFour t

to24 (Twelve h m ampm)
  | ampm == Am = TwentyFour (h * 100 + m)
  | ampm == Pm = TwentyFour ((h+12) * 100 + m)

equalTime :: Time -> Time -> Bool
equalTime t1 t2
  = to24 t1 == to24 t2
