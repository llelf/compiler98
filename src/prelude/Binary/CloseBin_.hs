module Binary
  ( closeBin
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))


gr_closeBin primitive 1 :: ForeignObj -> ()

closeBin :: BinHandle -> IO ()
closeBin (BH bh) =
  IO (\_ ->
	let gr_result = gr_closeBin bh
	in seq gr_result (Right gr_result))


