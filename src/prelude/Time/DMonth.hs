module Time where

data Month = January   | February | March    | April
           | May       | June     | July     | August
           | September | October  | November | December
           deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

