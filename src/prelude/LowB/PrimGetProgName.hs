module System where

import PackedString(PackedString,unpackPS)
import DIO

cGetProgName primitive 0 :: PackedString

primGetProgName =
  IO ( \ world ->
	 let pn = cGetProgName
	 in pn `seq` Right (unpackPS pn))
 
