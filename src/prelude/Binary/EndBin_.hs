module Binary
  ( endBin
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))
import BinPtr    ({-type-}BinPtr(..))


gr_endBin primitive 1 :: ForeignObj -> Int

endBin :: BinHandle -> IO BinPtr
endBin (BH bh) =
  IO (\_ ->
	let gr_result = gr_endBin bh
	    res1 = gr_result
	in seq gr_result (Right (BP res1)))


