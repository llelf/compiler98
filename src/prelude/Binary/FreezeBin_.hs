module Binary
  ( freezeBin
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))


gr_freezeBin primitive 1 :: ForeignObj -> ()

freezeBin :: BinHandle -> IO ()
freezeBin (BH bh) =
  IO (\_ ->
	let gr_result = gr_freezeBin bh
	in seq gr_result (Right gr_result))


