module CString(CString,PackedString,toCString,fromCString) where

import PackedString(PackedString,unpackPS,packString)

type CString = PackedString

toCString str = packString (str ++ ['\0'])
fromCString str = unpackPS str
