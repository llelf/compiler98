module CString(PackedString,toCString,fromCString) where

import PackedString(PackedString,unpackPS,packString)

toCString str = packString (str ++ ['\0'])
fromCString str = init (unpackPS str)
