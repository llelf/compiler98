module Binary
  ( alignBin
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))


gr_alignBin primitive 1 :: ForeignObj -> ()

alignBin :: BinHandle -> IO ()
alignBin (BH bh) =
  IO (\_ ->
	let gr_result = gr_alignBin bh
	in seq gr_result (Right gr_result))


