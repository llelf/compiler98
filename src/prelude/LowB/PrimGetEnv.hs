module System where

import PackedString(PackedString,unpackPS)
import CString
import DIO

cGetEnv primitive 1 :: CString -> PackedString

primGetEnv str =
  IO ( \ world ->
	   let args = cGetEnv (toCString str)
	   in args `seq` Right (unpackPS args) )

