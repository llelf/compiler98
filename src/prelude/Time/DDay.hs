module Time where

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday
         | Friday | Saturday
         deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)
