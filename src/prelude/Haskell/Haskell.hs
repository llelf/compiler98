module Haskell where

import Time
import Locale

systemName     :: String	-- "Hugs", "GHC", "hbc", "nhc", "qhc", ...
haskellVersion :: Int		-- 13, 14, 98, 2000, ...
releaseDate    :: CalendarTime	-- ...
version        :: String	-- some combination of the above

systemName     = "nhc"
haskellVersion = 98
releaseDate    = CalendarTime { ctYear=2000, ctMonth=September, ctDay=15,
                                ctHour=0, ctMin=0, ctSec=0, ctPicoSec=0,
                                ctWDay=Friday, ctYDay=200, ctTZName="UTC",
                                ctTZ=0, ctIsDST=False }
version        = systemName ++ show haskellVersion ++ " v1.00 " ++
                 formatCalendarTime defaultTimeLocale "%Y-%m-%d" releaseDate

