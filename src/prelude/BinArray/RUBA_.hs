module BinArray
  ( rUBA
  ) where

import GreenCard
import DUnboxedArray ({-type-}UnboxedArray(..))
import BinPtr        ({-type-}BinPtr(..))
import Bin           ({-type-}Bin(..))


gr_rUBA primitive 2 :: ForeignObj -> Int -> Int

rUBA :: UnboxedArray -> Int -> IO (Bin a)
rUBA (UBA uba) idx =
  IO (\_ ->
	let gr_result = gr_rUBA uba idx
	    res1 = gr_result
	in seq gr_result (Right (BP res1)))


