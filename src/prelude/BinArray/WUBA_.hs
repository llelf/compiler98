module BinArray
  ( wUBA
  ) where

import GreenCard
import DUnboxedArray ({-type-}UnboxedArray(..))
import BinPtr        ({-type-}BinPtr(..))
import Bin           ({-type-}Bin(..))


gr_wUBA primitive 3 :: ForeignObj -> Int -> Int -> Int

wUBA :: UnboxedArray -> (Bin a) -> (Bin a) -> IO Int
wUBA (UBA uba) (BP p) (BP end) =
  IO (\_ ->
	let gr_result = gr_wUBA uba p end
	in seq gr_result (Right gr_result))


