module Time where

data TimeDiff = TimeDiff {
    tdYear, tdMonth, tdDay, tdHour, tdMin, tdSec :: Int,
    tdPicoSec                                    :: Integer
    } deriving (Eq, Ord, Read, Show)

