module System where

import PackedString(PackedString,unpackPS)
import DIO

cGetArgs primitive 0 :: [PackedString]

primGetArgs =
  IO ( \ world ->
	let args = cGetArgs
	in args `seq` Right (map unpackPS args))
