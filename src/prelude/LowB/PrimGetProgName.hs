module System where

import NHC.PackedString(PackedString,unpackPS)
import NHC.Internal (IO(..))

cGetProgName primitive 0 :: PackedString

primGetProgName =
  IO ( \ world ->
	 let pn = cGetProgName
	 in pn `seq` Right (unpackPS pn))
 
