module Binary
  ( copyBytes
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))
import BinPtr    ({-type-}BinPtr(..))

    -- %-#include <stdio.h>


gr_copyBytes primitive 3 :: ForeignObj -> ForeignObj -> Int -> Int

copyBytes :: BinHandle -> BinHandle -> Int -> IO BinPtr
copyBytes (BH sbh) (BH dbh) bytes =
  IO (\_ ->
	let gr_result = gr_copyBytes sbh dbh bytes
	    destptr = gr_result
	in seq gr_result (Right (BP destptr)))


