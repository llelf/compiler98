module BinArray
  ( getUBAEnd
  ) where

import GreenCard
import DUnboxedArray ({-type-}UnboxedArray(..))
import BinPtr        ({-type-}BinPtr(..))


gr_getUBAEnd primitive 1 :: ForeignObj -> Int

getUBAEnd :: UnboxedArray -> IO BinPtr
getUBAEnd (UBA uba) =
  IO (\_ ->
	let gr_result = gr_getUBAEnd uba
	    res1 = gr_result
	in seq gr_result (Right (BP res1)))


