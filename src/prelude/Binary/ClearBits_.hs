module Binary
  ( clearBits
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))
import BinPtr    ({-type-}BinPtr(..))

    -- %-#include <stdio.h>

gr_clearBits primitive 2 :: ForeignObj -> Int -> Int

clearBits :: BinHandle -> Int -> IO BinPtr
clearBits (BH bh) width =
  IO (\_ ->
	let gr_result = gr_clearBits bh width
	    p = gr_result
	in seq gr_result (Right (BP p)))


