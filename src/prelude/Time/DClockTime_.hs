module Time
  ( ClockTime(..)
  ) where

import GreenCard

data ClockTime = CT Int deriving (Eq, Ord, Show)
