module Binary
  ( tellBin
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))
import BinPtr    ({-type-}BinPtr(..))


gr_tellBin primitive 1 :: ForeignObj -> Int

tellBin :: BinHandle -> IO BinPtr
tellBin (BH bh) =
  IO (\_ ->
	let gr_result = gr_tellBin bh
	    p = gr_result
	in seq gr_result (Right (BP p)))


