module BinArray
  ( getUBAFree
  ) where

import GreenCard
import DUnboxedArray ({-type-}UnboxedArray(..))


gr_getUBAFree primitive 1 :: ForeignObj -> Int

getUBAFree :: UnboxedArray -> IO Int
getUBAFree (UBA uba) =
  IO (\_ ->
	let gr_result = gr_getUBAFree uba
	in seq gr_result (Right gr_result))


