module BinArray
  ( allocUBA
  ) where

import GreenCard
import DUnboxedArray ({-type-}UnboxedArray(..))
import BinPtr        ({-type-}BinPtr(..))


gr_allocUBA primitive 2 :: Int -> Int -> ForeignObj

allocUBA :: Int -> BinPtr -> IO UnboxedArray
allocUBA size (BP end) =
  IO (\_ ->
	let gr_result = gr_allocUBA size end
	    uba = gr_result
	in seq gr_result (Right (UBA uba)))


