module IO where

data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                     deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)
