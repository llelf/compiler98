module System where

import PackedString(PackedString,unpackPS)
import CString
import DIO
import DIOError

cGetEnv primitive 1 :: PackedString -> Either Int PackedString

primGetEnv str =
  IO ( \ world ->
           let cstr = toCString str in
	   case cGetEnv cstr of
             Left errno -> Left (IOErrorSystem cstr errno)
             Right ps   -> Right (fromCString ps))

