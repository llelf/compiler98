module Binary
  ( putBits
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))
import BinPtr    ({-type-}BinPtr(..))

    -- %-#include <stdio.h>

gr_putBits primitive 3 :: ForeignObj -> Int -> Int -> Int

putBits :: BinHandle -> Int -> Int -> IO BinPtr
putBits (BH bh) width value =
  IO (\_ ->
	let gr_result = gr_putBits bh width value
	    p = gr_result
	in seq gr_result (Right (BP p)))


