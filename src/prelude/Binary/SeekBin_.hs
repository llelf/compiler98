module Binary
  ( seekBin
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))
import BinPtr    ({-type-}BinPtr(..))

    -- %-#include <stdio.h>

gr_seekBin primitive 2 :: ForeignObj -> Int -> ()

seekBin :: BinHandle -> BinPtr -> IO ()
seekBin (BH bh) (BP p) =
  IO (\_ ->
	let gr_result = gr_seekBin bh p
	in seq gr_result (Right gr_result))


