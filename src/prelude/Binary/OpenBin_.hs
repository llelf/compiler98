module Binary
  ( openBin
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))
import BinIOMode ({-type-}BinIOMode(..))
import BinLocation ({-type-}BinLocation(..), fromBinLocation)


gr_openBin primitive 3 :: Bool -> PackedString -> Int -> ForeignObj

openBin :: BinLocation -> IO BinHandle
openBin tmp1 =
  IO (\_ ->
	let (f,tmp2,tmp3) = fromBinLocation tmp1
	    fp = toCString tmp2
	    m = fromEnum tmp3
	    gr_result = gr_openBin f fp m
	    bh = gr_result
	in seq gr_result (Right (BH bh)))


