module Binary
  ( skipBits
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))


gr_skipBits primitive 2 :: ForeignObj -> Int -> ()

skipBits :: BinHandle -> Int -> IO ()
skipBits (BH bh) width =
  IO (\_ ->
	let gr_result = gr_skipBits bh width
	in seq gr_result (Right gr_result))


