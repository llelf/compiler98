module Time where

import DMonth
import DDay

data CalendarTime = CalendarTime {
    ctYear                       :: Int,
    ctMonth                      :: Month,
    ctDay, ctHour, ctMin, ctSec  :: Int,
    ctPicoSec                    :: Integer,
    ctWDay                       :: Day,
    ctYDay                       :: Int,
    ctTZName                     :: String,
    ctTZ                         :: Int,
    ctIsDST                      :: Bool
    } deriving (Eq, Ord, Read, Show)

