module Binary
  ( getBits
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))


gr_getBits primitive 2 :: ForeignObj -> Int -> Int

getBits :: BinHandle -> Int -> IO Int
getBits (BH bh) width =
  IO (\_ ->
	let gr_result = gr_getBits bh width
	in seq gr_result (Right gr_result))


