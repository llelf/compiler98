module Directory where

import GreenCard
import DClockTime ({-type-}ClockTime(..))


gr_getModificationTime primitive 1 :: PackedString -> Int

getModificationTime :: FilePath -> IO ClockTime
getModificationTime tmp1 =
  IO (\_ ->
	let fp = toCString tmp1
	    gr_result = gr_getModificationTime fp
	    res1 = gr_result
	in seq gr_result (Right (CT res1)))


