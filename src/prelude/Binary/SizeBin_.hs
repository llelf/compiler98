module Binary
  ( sizeBin
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))


gr_sizeBin primitive 1 :: ForeignObj -> Int

sizeBin :: BinHandle -> IO Int
sizeBin (BH bh) =
  IO (\_ ->
	let gr_result = gr_sizeBin bh
	in seq gr_result (Right gr_result))


