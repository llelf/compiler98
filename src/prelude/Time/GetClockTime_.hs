module Time
  ( getClockTime
  ) where

import GreenCard
import DClockTime


gr_getClockTime primitive 0 :: Int

getClockTime :: IO ClockTime
getClockTime =
  IO (\_ ->
	let gr_result = gr_getClockTime
	    c = gr_result
	in seq gr_result (Right (CT c)))


